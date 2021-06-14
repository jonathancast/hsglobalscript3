{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module GSI.Functions (gslist, gslist_w, gsstring, gsstring_w, gslazylist, gslazylist_w, gslazystring, gslazystring_w, gsbool, gsapiEval, gsapiEvalPos, gsapiEvalList, gsapiEvalString, gsapiEvalNatural, gsapiEvalExternal, gsfmterrormsg) where

import Data.Proxy (Proxy(..))

import Control.Exception (Exception(..), throwIO, try)

import Language.Haskell.TH.Lib (appE, varE)

import qualified Data.Map as Map

import GSI.Util (Pos(..), StackTrace(..), gshere, fmtPos)
import GSI.Syn (gsvar, fmtVarAtom, fmtVarBindAtom)
import GSI.Message (Message)
import GSI.Prof (ProfCounter)
import GSI.RTS (OPort)
import GSI.Value (GSValue(..), GSExpr(..), GSExprCont(..), GSExternal(..), GSError(..), GSInvalidProgram(..), GSException(..), gsundefined_value, gsimplementationfailure, gsapply, gsfield, gsthunk_w, fmtExternal, fmtError, whichExternal, gsvCode)
import GSI.Eval (evalSync)
import API (apiImplementationFailure)

gslist = varE 'gslist_w `appE` gshere

gslist_w :: Pos -> [GSValue] -> GSValue
gslist_w pos [] = GSConstr pos (gsvar "nil") []
gslist_w pos (x:xn) = GSConstr pos (gsvar ":") [ x, gslist_w pos xn ]

gslazylist = varE 'gslazylist_w `appE` gshere

gslazylist_w :: Pos -> [GSValue] -> IO GSValue
gslazylist_w pos xn = gsthunk_w pos $ GSExpr $ \ evs cs sk -> case xn of
    [] -> gsreturn sk (GSConstr pos (gsvar "nil") [])
    x:xn1 -> do
        xn1v <- gslazylist_w pos xn1
        gsreturn sk (GSConstr pos (gsvar ":") [ x, xn1v ])

gsstring = varE 'gsstring_w `appE` gshere

gsstring_w :: Pos -> String -> GSValue
gsstring_w pos s = gslist_w pos $ map GSRune s

gslazystring = varE 'gslazystring_w `appE` gshere

gslazystring_w :: Pos -> String -> IO GSValue
gslazystring_w pos s = gslazylist_w pos $ map GSRune s

gsbool b = case b of
    False -> GSConstr $gshere (gsvar "false") []
    True -> GSConstr $gshere (gsvar "true") []

gsevalChar :: OPort Message -> Maybe ProfCounter -> Pos -> GSValue -> IO Char
gsevalChar msg pc pos (GSThunk th) = do
    v <- evalSync msg pc [StackTrace pos []] th
    gsevalChar msg pc pos v
gsevalChar msg pc pos (GSRune ch) = return ch
gsevalChar msg pc pos v =
    throwIO $ GSExcImplementationFailure $gshere $ "gsevalChar " ++ gsvCode v ++ " next"

gsevalNatural :: OPort Message -> Maybe ProfCounter -> Pos -> GSValue -> IO Integer
gsevalNatural msg pc pos (GSThunk th) = do
    v <- evalSync msg pc [StackTrace pos []] th
    gsevalNatural msg pc pos v
gsevalNatural msg pc pos (GSNatural _ n) = return n
gsevalNatural msg pc pos v =
    throwIO $ GSExcImplementationFailure $gshere $ "gsevalNatural " ++ gsvCode v ++ " next"

gsevalList :: OPort Message -> Maybe ProfCounter -> Pos -> GSValue -> IO [GSValue]
gsevalList msg pc pos v = gsevalList_w msg pc pos id v where
    gsevalList_w msg pc pos ds (GSThunk ts) = do
        v <- evalSync msg pc  [StackTrace pos []] ts
        gsevalList_w msg pc pos ds v
    gsevalList_w msg pc pos ds (GSError err) = throwIO $ GSExcError err
    gsevalList_w msg pc pos ds (GSConstr pos1 c [x, xn]) | c == gsvar ":" = gsevalList_w msg pc pos (ds . (x:)) xn
    gsevalList_w msg pc pos ds (GSConstr pos1 c []) | c == gsvar "nil" = return (ds [])
    gsevalList_w msg pc pos ds (GSConstr pos1 c as) = throwIO $ GSExcImplementationFailure $gshere $ "gsevalList " ++ fmtVarAtom c " next"
    gsevalList_w msg pc pos ds v = throwIO $ GSExcImplementationFailure $gshere $ "gsevalList " ++ gsvCode v ++ " next"

gsapiEval :: OPort Message -> Maybe ProfCounter -> Pos -> GSValue -> IO GSValue
gsapiEval msg pc pos (GSThunk th) = do
    v' <- evalSync msg pc [StackTrace pos []] th
    gsapiEval msg pc pos v'
gsapiEval msg pc pos v@GSRecord{} = return v
gsapiEval msg pc pos v = $apiImplementationFailure $ "gsapiEval " ++ gsvCode v ++ " next"

gsapiEvalPos :: OPort Message -> Maybe ProfCounter -> Pos -> GSValue -> IO Pos
gsapiEvalPos msg pc pos (GSThunk th) = do
    v' <- evalSync msg pc [StackTrace pos []] th
    gsapiEvalPos msg pc pos v'
gsapiEvalPos msg pc pos v@GSRecord{} = do
    filename <- gsapiEvalString msg pc pos =<< $gsfield (gsvar "filename") v
    line <- gsapiEvalNatural msg pc pos =<< $gsfield (gsvar "line") v
    col <- gsapiEvalNatural msg pc pos =<< $gsfield (gsvar "col") v
    return $ Pos filename line col
gsapiEvalPos msg pc pos v = $apiImplementationFailure $ "gsapiEvalPos " ++ gsvCode v ++ " next"

gsevalString :: OPort Message -> Maybe ProfCounter -> Pos -> GSValue -> IO String
gsevalString msg pc pos v = gsevalString_w msg pc pos id v where
    gsevalString_w msg pc pos ds (GSError err) = throwIO $ GSExcError err
    gsevalString_w msg pc pos ds (GSThunk th) = do
        v <- evalSync msg pc [StackTrace pos []] th
        gsevalString_w msg pc pos ds v
    gsevalString_w msg pc pos ds (GSConstr pos1 c [ chv, sv ]) | c == gsvar ":" = do
        ch <- gsevalChar msg pc pos chv
        gsevalString_w msg pc pos (ds . (ch:)) sv
    gsevalString_w msg pc pos ds (GSConstr pos1 c []) | c == gsvar "nil" = return $ ds ""
    gsevalString_w msg pc pos ds (GSConstr pos1 c as) =
        throwIO $ GSExcImplementationFailure $gshere $ "gsevalString " ++ fmtVarAtom c " next"
    gsevalString_w msg pc pos ds v =
        throwIO $ GSExcImplementationFailure $gshere $ "gsevalString " ++ gsvCode v ++ " next"

gsevalExternal :: forall a. GSExternal a => OPort Message -> Maybe ProfCounter -> Pos -> GSValue -> IO a
gsevalExternal msg pc pos (GSThunk ts) = do
    v <- evalSync msg pc [StackTrace pos []] ts
    gsevalExternal msg pc pos v
gsevalExternal msg pc pos (GSError err) = throwIO $ GSExcError err
gsevalExternal msg pc pos (GSInvalidProgram ip) = throwIO $ GSExcInvalidProgram ip
gsevalExternal msg pc pos (GSExternal e) = case fromExternal e of
    Nothing -> throwIO $ GSExcInvalidProgram $ GSIPRuntimeTypeError (StackTrace pos []) "gsevalExternal" (whichExternal e) (externalType (Proxy :: Proxy a))
    Just x -> return x
gsevalExternal msg pc _ (GSImplementationFailure pos1 err) = throwIO $ GSExcImplementationFailure pos1 err
gsevalExternal msg pc pos v = throwIO $ GSExcImplementationFailure $gshere $ "gsevalExternal " ++ gsvCode v ++ " next"

gsapiEvalList :: OPort Message -> Maybe ProfCounter -> Pos -> GSValue -> IO [GSValue]
gsapiEvalList msg pc pos xnv = gsevalForApi $ gsevalList msg pc pos xnv

gsapiEvalString :: OPort Message -> Maybe ProfCounter -> Pos -> GSValue -> IO String
gsapiEvalString msg pc pos fnv = gsevalForApi $ gsevalString msg pc pos fnv

gsapiEvalNatural :: OPort Message -> Maybe ProfCounter -> Pos -> GSValue -> IO Integer
gsapiEvalNatural msg pc pos fnv = gsevalForApi $ gsevalNatural msg pc pos fnv

gsapiEvalExternal :: GSExternal a => OPort Message -> Maybe ProfCounter -> Pos -> GSValue -> IO a
gsapiEvalExternal msg pc pos v = gsevalForApi $ gsevalExternal msg pc pos v

gsevalForApi :: IO a -> IO a
gsevalForApi ev = ev

gsfmterrormsg = varE 'gsfmterrormsg_w `appE` gshere

gsfmterrormsg_w :: Pos -> OPort Message -> GSValue -> IO String
gsfmterrormsg_w pos msg msgv = do
    msgt <- $gsapply msgv [ GSRecord $gshere (Map.fromList [
        (gsvar "paragraph-constituents", GSConstr $gshere (gsvar "nil") [])
      ]) ]
    msg_pcs <- $gsfield (gsvar "paragraph-constituents") msgt
    gsfmterrormsg_ww msg pos id msg_pcs

gsfmterrormsg_ww :: OPort Message -> Pos -> (String -> String) -> GSValue -> IO String
gsfmterrormsg_ww msg pos ds (GSThunk th) = do
    v <- evalSync msg Nothing [StackTrace pos []] th
    gsfmterrormsg_ww msg pos ds v
gsfmterrormsg_ww msg pos ds (GSError err) = return $ (ds . ("<Error: "++) . (fmtError err++) . ('>':)) $ ""
gsfmterrormsg_ww _ pos0 ds (GSImplementationFailure pos1 msg) = return $ (ds . ("<Implementation Failure: "++) . (fmtPos pos1) . (msg++) . ('>':)) $ ""
gsfmterrormsg_ww msg pos0 ds (GSConstr pos1 c [ GSThunk pcth, msg1 ]) | c == gsvar ":" = do
    pcv <- evalSync msg Nothing [StackTrace pos0 []] pcth
    gsfmterrormsg_ww msg pos0 ds (GSConstr pos1 c [ pcv, msg1 ])
gsfmterrormsg_ww msg pos0 ds (GSConstr pos1 c [ GSError err, msg1 ]) | c == gsvar ":" =
    gsfmterrormsg_ww msg pos0 (ds . ("<Error: "++) . (fmtError err++) . ('>':)) msg1
gsfmterrormsg_ww msg pos0 ds (GSConstr pos1 c1 [ GSConstr pos2 c2 [ GSRune ch ], msg1 ]) | c1 == gsvar ":" && c2 == gsvar "char" =
    gsfmterrormsg_ww msg pos0 (ds . (ch:)) msg1
gsfmterrormsg_ww msg pos0 ds (GSConstr pos1 c1 [ GSConstr pos2 c2 [ ch ], msg1 ]) | c1 == gsvar ":" && c2 == gsvar "char" =
    gsfmterrormsg_ww msg pos0 (ds . ('<':) . fmtPos $gshere . ("gsfmterrormsg (char "++) . (gsvCode ch++) . (" : _"++) . (") next"++) . ('>':)) msg1
gsfmterrormsg_ww msg pos0 ds (GSConstr pos1 c1 [ GSConstr pos2 c2 [ x ], msg1 ]) | c1 == gsvar ":" && c2 == gsvar "gsv" = do
    xds <- gsfmterrorvalue msg pos0 x
    gsfmterrormsg_ww msg pos0 (ds . xds) msg1
gsfmterrormsg_ww msg pos0 ds (GSConstr pos1 c1 [ GSConstr pos2 c2 _, msg1 ]) | c1 == gsvar ":" =
    gsfmterrormsg_ww msg pos0 (ds . ('<':) . fmtPos $gshere . ("gsfmterrormsg ("++) . fmtVarAtom c2 . (" : _"++) . (") next"++) . ('>':)) msg1
gsfmterrormsg_ww msg pos0 ds (GSConstr pos1 c [ c0, msg1 ]) | c == gsvar ":" =
    gsfmterrormsg_ww msg pos0 (ds . ('<':) . fmtPos $gshere . ("gsfmterrormsg ("++) . (gsvCode c0++) . (" : _"++) . (") next"++) . ('>':)) msg1
gsfmterrormsg_ww msg pos0 ds (GSConstr pos1 c []) | c== gsvar "nil" = return $ ds $ ""
gsfmterrormsg_ww msg pos0 ds (GSConstr pos1 c as) =
    return $ (ds . ('<':) . fmtPos $gshere . ("gsfmterrormsg "++) . fmtVarAtom c . (" next"++) . ('>':)) $ ""
gsfmterrormsg_ww _ pos ds msg =
    return $ (ds . ('<':) . fmtPos $gshere . ("gsfmterrormsg "++) . (gsvCode msg++) . (" next"++) . ('>':)) $ ""

-- §begin§itemize
--     §item Evaluates thunks
--     §item Prints errors
-- §end
gsfmterrorvalue :: OPort Message -> Pos -> GSValue -> IO (String -> String)
gsfmterrorvalue msg pos (GSThunk th) = do
    v <- evalSync msg Nothing [StackTrace pos []] th
    gsfmterrorvalue msg pos v
gsfmterrorvalue msg pos v@GSImplementationFailure{} = gsfmterrorvalueAtom msg pos v
gsfmterrorvalue msg pos v@GSError{} = gsfmterrorvalueAtom msg pos v
gsfmterrorvalue msg pos (GSExternal e) | Just v <- fromExternal e = return $ ("<gsvar "++) . fmtVarAtom v . ('>':)
gsfmterrorvalue msg pos v@GSRecord{} = gsfmterrorvalueAtom msg pos v
gsfmterrorvalue msg pos v@(GSConstr pos1 c [ GSRune{}, _ ]) | c == gsvar ":" = gsfmterrorvalueAtom msg pos v
gsfmterrorvalue msg pos (GSConstr pos1 c as) = foldl (\ s s' -> s . (' ':) . s') (fmtVarAtom c) <$> mapM (gsfmterrorvalueAtom msg pos) as
gsfmterrorvalue msg pos x = return $ ('<':) . fmtPos $gshere . ("gsfmterrorvalue "++) . (gsvCode x++) . (" next"++) . ('>':)

gsfmterrorvalueAtom :: OPort Message -> Pos -> GSValue -> IO (String -> String)
gsfmterrorvalueAtom msg pos (GSThunk th) = do
    v <- evalSync msg Nothing [StackTrace pos []] th
    gsfmterrorvalueAtom msg pos v
gsfmterrorvalueAtom _ pos0 (GSImplementationFailure pos1 msg) = return $ ("<Implementation Failure: "++) . (fmtPos pos1) . (msg++) . ('>':)
gsfmterrorvalueAtom msg pos0 (GSError err) = return $ ('<':) . (fmtError err ++) . ('>':)
gsfmterrorvalueAtom msg pos0 (GSRune ch) | not (ch `elem` "\\/§()[]{}") = return $ ("r/"++) . (ch:) . ("/"++)
gsfmterrorvalueAtom msg pos v@(GSConstr pos1 c [ GSRune{}, _ ]) | c == gsvar ":" = gsfmterrorString msg pos ("qq{"++) v
gsfmterrorvalueAtom msg pos v@(GSConstr pos1 c []) = return $ fmtVarAtom c
gsfmterrorvalueAtom msg pos v@GSConstr{} = gsfmterrorvalue msg pos v >>= \ ds -> return $ ('(':) . ds . (')':)
gsfmterrorvalueAtom msg pos (GSRecord pos1 m) = do
    vdss <- mapM
        (\ (x, v) -> do
            vds <- gsfmterrorvalue msg pos1 v
            return $ fmtVarBindAtom x . (" ∝ "++) . vds . ("; "++)
        )
        (Map.toList m)
    return $ ('〈':) . (case vdss of [] -> id; _ -> (' ':)) . foldr (.) id vdss . ('〉':)
gsfmterrorvalueAtom msg pos (GSExternal e) = fmtExternal e >>= \ ds -> return $ ('<':) . ds . ('>':)
gsfmterrorvalueAtom msg pos x = return $ ('<':) . fmtPos $gshere . ("gsfmterrorvalueAtom "++) . (gsvCode x++) . (" next"++) . ('>':)

gsfmterrorString :: OPort Message -> Pos -> (String -> String) -> GSValue -> IO (String -> String)
gsfmterrorString msg pos ds (GSThunk th) = do
    v <- evalSync msg Nothing [StackTrace pos []] th
    gsfmterrorString msg pos ds v
gsfmterrorString msg pos ds v@(GSConstr pos1 c [ GSRune ch, s ]) | c == gsvar ":" && ch `elem` "\\§()[]{}" = gsfmterrorString msg pos (ds . ('\\':) . (ch:)) s
gsfmterrorString msg pos ds v@(GSConstr pos1 c [ GSRune ch, s ]) | c == gsvar ":" = gsfmterrorString msg pos (ds . (ch:)) s
gsfmterrorString msg pos ds v@(GSConstr pos1 c [ ch, s ]) | c == gsvar ":"  =
    gsfmterrorString msg pos (ds . ("§(<"++) . fmtPos $gshere . ("gsfmterrorString "++) . (gsvCode ch++) . (" : _ next"++) . (">)"++)) s
gsfmterrorString msg pos ds (GSConstr pos1 c []) | c == gsvar "nil" = gsfmterrorStringTerm ds
gsfmterrorString msg pos ds (GSConstr pos1 c _) =
    gsfmterrorStringTerm $ ds . ("§(<"++) . fmtPos $gshere . ("gsfmterrorString "++) . (fmtVarAtom c) . (" next"++) . (">)"++)
gsfmterrorString msg pos ds x = gsfmterrorStringTerm $ ds . ("§(<"++) . fmtPos $gshere . ("gsfmterrorString "++) . (gsvCode x++) . (" next"++) . (">)"++)

gsfmterrorStringExpr :: OPort Message -> Pos -> (String -> String) -> GSValue -> IO (String -> String)
gsfmterrorStringExpr msg pos ds v = do
    ds1 <- gsfmterrorvalue msg pos v
    gsfmterrorStringTerm $ ds . ("§("++) . ds1 . (')':)

gsfmterrorStringTerm :: (String -> String) -> IO (String -> String)
gsfmterrorStringTerm ds = return $ ds . ('}':)

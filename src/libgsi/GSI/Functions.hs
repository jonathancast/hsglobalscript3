{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module GSI.Functions (gslist, gslist_w, gsstring, gsstring_w, gslazylist, gslazylist_w, gslazystring, gslazystring_w, gsbool, gsapiEval, gsapiEvalPos, gsapiEvalList, gsapiEvalString, gsapiEvalNatural, gsapiEvalExternal, gsfmtException, fmtInvalidProgram, fmtInvalidProgramShort, fmtError, fmtErrorShort, gsfmterrormsg) where

import Data.Proxy (Proxy(..))

import Control.Exception (throwIO)

import Language.Haskell.TH.Lib (appE, varE)

import qualified Data.Map as Map

import GSI.Util (Pos(..), StackTrace(..), gshere, fmtPos, fmtStackTrace)
import GSI.Syn (gsvar, fmtVarAtom, fmtVarBindAtom)
import GSI.Value (GSValue(..), GSExpr(..), GSEvalState(..), GSExprCont(..), GSExternal(..), GSError(..), GSInvalidProgram(..), GSException(..), GSEvalState(..), gsapply, gsfield, gsthunk_w, fmtExternal, whichExternal, gsvCode)
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

gsevalChar :: GSEvalState -> Pos -> GSValue -> IO Char
gsevalChar evs pos (GSThunk th) = do
    v <- evalSync (msgChannel evs) (profCounter evs) [StackTrace pos []] th
    gsevalChar evs pos v
gsevalChar evs pos (GSRune ch) = return ch
gsevalChar evs pos v =
    throwIO $ GSExcImplementationFailure $gshere $ "gsevalChar " ++ gsvCode v ++ " next"

gsevalNatural :: GSEvalState -> Pos -> GSValue -> IO Integer
gsevalNatural evs pos (GSThunk th) = do
    v <- evalSync (msgChannel evs) (profCounter evs) [StackTrace pos []] th
    gsevalNatural evs pos v
gsevalNatural evs pos (GSNatural _ n) = return n
gsevalNatural evs pos v =
    throwIO $ GSExcImplementationFailure $gshere $ "gsevalNatural " ++ gsvCode v ++ " next"

gsevalList :: GSEvalState -> Pos -> GSValue -> IO [GSValue]
gsevalList evs pos v = gsevalList_w pos id v where
    gsevalList_w pos ds (GSThunk ts) = do
        v <- evalSync (msgChannel evs) (profCounter evs)  [StackTrace pos []] ts
        gsevalList_w pos ds v
    gsevalList_w pos ds (GSError err) = throwIO $ GSExcError err
    gsevalList_w pos ds (GSConstr pos1 c [x, xn]) | c == gsvar ":" = gsevalList_w pos (ds . (x:)) xn
    gsevalList_w pos ds (GSConstr pos1 c []) | c == gsvar "nil" = return (ds [])
    gsevalList_w pos ds (GSConstr pos1 c as) = throwIO $ GSExcImplementationFailure $gshere $ "gsevalList " ++ fmtVarAtom c " next"
    gsevalList_w pos ds v = throwIO $ GSExcImplementationFailure $gshere $ "gsevalList " ++ gsvCode v ++ " next"

gsapiEval :: GSEvalState -> Pos -> GSValue -> IO GSValue
gsapiEval evs pos (GSThunk th) = do
    v' <- evalSync (msgChannel evs) (profCounter evs) [StackTrace pos []] th
    gsapiEval evs pos v'
gsapiEval evs pos v@GSRecord{} = return v
gsapiEval evs pos v = $apiImplementationFailure $ "gsapiEval " ++ gsvCode v ++ " next"

gsapiEvalPos :: GSEvalState -> Pos -> GSValue -> IO Pos
gsapiEvalPos evs pos (GSThunk th) = do
    v' <- evalSync (msgChannel evs) (profCounter evs) [StackTrace pos []] th
    gsapiEvalPos evs pos v'
gsapiEvalPos evs pos v@GSRecord{} = do
    filename <- gsapiEvalString evs pos =<< $gsfield (gsvar "filename") v
    line <- gsapiEvalNatural evs pos =<< $gsfield (gsvar "line") v
    col <- gsapiEvalNatural evs pos =<< $gsfield (gsvar "col") v
    return $ Pos filename line col
gsapiEvalPos evs pos v = $apiImplementationFailure $ "gsapiEvalPos " ++ gsvCode v ++ " next"

gsevalString :: GSEvalState -> Pos -> GSValue -> IO String
gsevalString evs pos v = gsevalString_w pos id v where
    gsevalString_w pos ds (GSError err) = throwIO $ GSExcError err
    gsevalString_w pos ds (GSThunk th) = do
        v <- evalSync (msgChannel evs) (profCounter evs) [StackTrace pos []] th
        gsevalString_w pos ds v
    gsevalString_w pos ds (GSConstr pos1 c [ chv, sv ]) | c == gsvar ":" = do
        ch <- gsevalChar evs pos chv
        gsevalString_w pos (ds . (ch:)) sv
    gsevalString_w pos ds (GSConstr pos1 c []) | c == gsvar "nil" = return $ ds ""
    gsevalString_w pos ds (GSConstr pos1 c as) =
        throwIO $ GSExcImplementationFailure $gshere $ "gsevalString " ++ fmtVarAtom c " next"
    gsevalString_w pos ds v =
        throwIO $ GSExcImplementationFailure $gshere $ "gsevalString " ++ gsvCode v ++ " next"

gsevalExternal :: forall a. GSExternal a => GSEvalState -> Pos -> GSValue -> IO a
gsevalExternal evs pos (GSThunk ts) = do
    v <- evalSync (msgChannel evs) (profCounter evs) [StackTrace pos []] ts
    gsevalExternal evs pos v
gsevalExternal evs pos (GSError err) = throwIO $ GSExcError err
gsevalExternal evs pos (GSInvalidProgram ip) = throwIO $ GSExcInvalidProgram ip
gsevalExternal evs pos (GSExternal e) = case fromExternal e of
    Nothing -> throwIO $ GSExcInvalidProgram $ GSIPRuntimeTypeError (StackTrace pos []) "gsevalExternal" (whichExternal e) (externalType (Proxy :: Proxy a))
    Just x -> return x
gsevalExternal evs _ (GSImplementationFailure pos1 err) = throwIO $ GSExcImplementationFailure pos1 err
gsevalExternal evs pos v = throwIO $ GSExcImplementationFailure $gshere $ "gsevalExternal " ++ gsvCode v ++ " next"

gsapiEvalList :: GSEvalState -> Pos -> GSValue -> IO [GSValue]
gsapiEvalList evs pos xnv = gsevalForApi $ gsevalList evs pos xnv

gsapiEvalString :: GSEvalState -> Pos -> GSValue -> IO String
gsapiEvalString evs pos fnv = gsevalForApi $ gsevalString evs pos fnv

gsapiEvalNatural :: GSEvalState -> Pos -> GSValue -> IO Integer
gsapiEvalNatural evs pos fnv = gsevalForApi $ gsevalNatural evs pos fnv

gsapiEvalExternal :: GSExternal a => GSEvalState -> Pos -> GSValue -> IO a
gsapiEvalExternal evs pos v = gsevalForApi $ gsevalExternal evs pos v

gsevalForApi :: IO a -> IO a
gsevalForApi ev = ev

gsfmtException :: GSEvalState -> GSException -> IO String
gsfmtException evs (GSExcError e) = fmtError e
gsfmtException evs (GSExcInvalidProgram ip) = return $ fmtInvalidProgram ip
gsfmtException evs (GSExcImplementationFailure pos err) = return $ fmtPos pos err
gsfmtException evs (GSExcAbend pos err) = return $ fmtPos pos err

fmtInvalidProgram :: GSInvalidProgram -> String
fmtInvalidProgram (GSIPRuntimeTypeError st ctxt act exp) = fmtStackTrace st $ "In " ++ ctxt ++ ", found " ++ act ++ "; expected " ++ exp

fmtInvalidProgramShort :: GSInvalidProgram -> String
fmtInvalidProgramShort (GSIPRuntimeTypeError (StackTrace pos _) ctxt act exp) = fmtPos pos $ "In " ++ ctxt ++ ", found " ++ act ++ "; expected " ++ exp

fmtError :: GSError -> IO String
fmtError (GSErrUnimpl st) = return $ fmtStackTrace st "Undefined"
fmtError (GSErrUnimplField pos f) = return $ fmtPos pos . ("Undefined field "++) . fmtVarAtom f $ ""
fmtError (GSErrInsufficientCases pos err) = return $ fmtPos pos $ "Missing case: " ++ err
fmtError (GSErrError pos err) = return $ fmtPos pos $ "Error: " ++ err

fmtErrorShort :: GSError -> IO String
fmtErrorShort (GSErrUnimpl (StackTrace pos _)) = return $ fmtPos pos "Undefined"
fmtErrorShort (GSErrUnimplField pos f) = return $ fmtPos pos . ("Undefined field "++) . fmtVarAtom f $ ""
fmtErrorShort (GSErrInsufficientCases pos err) = return $ fmtPos pos $ "Missing case: " ++ err
fmtErrorShort (GSErrError pos err) = return $ fmtPos pos $ "Error: " ++ err

gsfmterrormsg = varE 'gsfmterrormsg_w `appE` gshere

gsfmterrormsg_w :: Pos -> GSEvalState -> GSValue -> IO String
gsfmterrormsg_w pos evs msgv = do
    msgt <- $gsapply msgv [ GSRecord $gshere (Map.fromList [
        (gsvar "paragraph-constituents", GSConstr $gshere (gsvar "nil") [])
      ]) ]
    msg_pcs <- $gsfield (gsvar "paragraph-constituents") msgt
    gsfmterrormsg_ww evs pos id msg_pcs

gsfmterrormsg_ww :: GSEvalState -> Pos -> (String -> String) -> GSValue -> IO String
gsfmterrormsg_ww evs pos ds (GSThunk th) = do
    v <- evalSync (msgChannel evs) Nothing [StackTrace pos []] th
    gsfmterrormsg_ww evs pos ds v
gsfmterrormsg_ww evs pos ds (GSError err) = do errs <- fmtError err; return $ (ds . ("<Error: "++) . (errs++) . ('>':)) $ ""
gsfmterrormsg_ww _ pos0 ds (GSImplementationFailure pos1 msg) = return $ (ds . ("<Implementation Failure: "++) . (fmtPos pos1) . (msg++) . ('>':)) $ ""
gsfmterrormsg_ww evs pos0 ds (GSConstr pos1 c [ GSThunk pcth, msg1 ]) | c == gsvar ":" = do
    pcv <- evalSync (msgChannel evs) Nothing [StackTrace pos0 []] pcth
    gsfmterrormsg_ww evs pos0 ds (GSConstr pos1 c [ pcv, msg1 ])
gsfmterrormsg_ww evs pos0 ds (GSConstr pos1 c [ GSError err, msg1 ]) | c == gsvar ":" = do
    errs <- fmtError err
    gsfmterrormsg_ww evs pos0 (ds . ("<Error: "++) . (errs++) . ('>':)) msg1
gsfmterrormsg_ww evs pos0 ds (GSConstr pos1 c1 [ GSConstr pos2 c2 [ GSRune ch ], msg1 ]) | c1 == gsvar ":" && c2 == gsvar "char" =
    gsfmterrormsg_ww evs pos0 (ds . (ch:)) msg1
gsfmterrormsg_ww evs pos0 ds (GSConstr pos1 c1 [ GSConstr pos2 c2 [ ch ], msg1 ]) | c1 == gsvar ":" && c2 == gsvar "char" =
    gsfmterrormsg_ww evs pos0 (ds . ('<':) . fmtPos $gshere . ("gsfmterrormsg (char "++) . (gsvCode ch++) . (" : _"++) . (") next"++) . ('>':)) msg1
gsfmterrormsg_ww evs pos0 ds (GSConstr pos1 c1 [ GSConstr pos2 c2 [ x ], msg1 ]) | c1 == gsvar ":" && c2 == gsvar "gsv" = do
    xds <- gsfmterrorvalue evs pos0 x
    gsfmterrormsg_ww evs pos0 (ds . xds) msg1
gsfmterrormsg_ww evs pos0 ds (GSConstr pos1 c1 [ GSConstr pos2 c2 _, msg1 ]) | c1 == gsvar ":" =
    gsfmterrormsg_ww evs pos0 (ds . ('<':) . fmtPos $gshere . ("gsfmterrormsg ("++) . fmtVarAtom c2 . (" : _"++) . (") next"++) . ('>':)) msg1
gsfmterrormsg_ww evs pos0 ds (GSConstr pos1 c [ c0, msg1 ]) | c == gsvar ":" =
    gsfmterrormsg_ww evs pos0 (ds . ('<':) . fmtPos $gshere . ("gsfmterrormsg ("++) . (gsvCode c0++) . (" : _"++) . (") next"++) . ('>':)) msg1
gsfmterrormsg_ww evs pos0 ds (GSConstr pos1 c []) | c== gsvar "nil" = return $ ds $ ""
gsfmterrormsg_ww evs pos0 ds (GSConstr pos1 c as) =
    return $ (ds . ('<':) . fmtPos $gshere . ("gsfmterrormsg "++) . fmtVarAtom c . (" next"++) . ('>':)) $ ""
gsfmterrormsg_ww _ pos ds msg =
    return $ (ds . ('<':) . fmtPos $gshere . ("gsfmterrormsg "++) . (gsvCode msg++) . (" next"++) . ('>':)) $ ""

-- §begin§itemize
--     §item Evaluates thunks
--     §item Prints errors
-- §end
gsfmterrorvalue :: GSEvalState -> Pos -> GSValue -> IO (String -> String)
gsfmterrorvalue evs pos (GSThunk th) = do
    v <- evalSync (msgChannel evs) Nothing [StackTrace pos []] th
    gsfmterrorvalue evs pos v
gsfmterrorvalue evs pos v@GSImplementationFailure{} = gsfmterrorvalueAtom evs pos v
gsfmterrorvalue evs pos v@GSError{} = gsfmterrorvalueAtom evs pos v
gsfmterrorvalue evs pos (GSExternal e) | Just v <- fromExternal e = return $ ("<gsvar "++) . fmtVarAtom v . ('>':)
gsfmterrorvalue evs pos v@GSRecord{} = gsfmterrorvalueAtom evs pos v
gsfmterrorvalue evs pos v@(GSConstr pos1 c [ GSRune{}, _ ]) | c == gsvar ":" = gsfmterrorvalueAtom evs pos v
gsfmterrorvalue evs pos (GSConstr pos1 c as) = foldl (\ s s' -> s . (' ':) . s') (fmtVarAtom c) <$> mapM (gsfmterrorvalueAtom evs pos) as
gsfmterrorvalue evs pos x = return $ ('<':) . fmtPos $gshere . ("gsfmterrorvalue "++) . (gsvCode x++) . (" next"++) . ('>':)

gsfmterrorvalueAtom :: GSEvalState -> Pos -> GSValue -> IO (String -> String)
gsfmterrorvalueAtom evs pos (GSThunk th) = do
    v <- evalSync (msgChannel evs) Nothing [StackTrace pos []] th
    gsfmterrorvalueAtom evs pos v
gsfmterrorvalueAtom _ pos0 (GSImplementationFailure pos1 msg) = return $ ("<Implementation Failure: "++) . (fmtPos pos1) . (msg++) . ('>':)
gsfmterrorvalueAtom evs pos0 (GSError err) = do errs <- fmtError err; return $ ('<':) . (errs ++) . ('>':)
gsfmterrorvalueAtom evs pos0 (GSRune ch) | not (ch `elem` "\\/§()[]{}") = return $ ("r/"++) . (ch:) . ("/"++)
gsfmterrorvalueAtom evs pos v@(GSConstr pos1 c [ GSRune{}, _ ]) | c == gsvar ":" = gsfmterrorString evs{profCounter = Nothing} pos ("qq{"++) v
gsfmterrorvalueAtom evs pos v@(GSConstr pos1 c []) = return $ fmtVarAtom c
gsfmterrorvalueAtom evs pos v@GSConstr{} = gsfmterrorvalue evs{profCounter = Nothing} pos v >>= \ ds -> return $ ('(':) . ds . (')':)
gsfmterrorvalueAtom evs pos (GSRecord pos1 m) = do
    vdss <- mapM
        (\ (x, v) -> do
            vds <- gsfmterrorvalue evs{profCounter = Nothing} pos1 v
            return $ fmtVarBindAtom x . (" ∝ "++) . vds . ("; "++)
        )
        (Map.toList m)
    return $ ('〈':) . (case vdss of [] -> id; _ -> (' ':)) . foldr (.) id vdss . ('〉':)
gsfmterrorvalueAtom evs pos (GSExternal e) = fmtExternal e >>= \ ds -> return $ ('<':) . ds . ('>':)
gsfmterrorvalueAtom evs pos x = return $ ('<':) . fmtPos $gshere . ("gsfmterrorvalueAtom "++) . (gsvCode x++) . (" next"++) . ('>':)

gsfmterrorString :: GSEvalState -> Pos -> (String -> String) -> GSValue -> IO (String -> String)
gsfmterrorString evs pos ds (GSThunk th) = do
    v <- evalSync (msgChannel evs) Nothing [StackTrace pos []] th
    gsfmterrorString evs pos ds v
gsfmterrorString evs pos ds v@(GSConstr pos1 c [ GSRune ch, s ]) | c == gsvar ":" && ch `elem` "\\§()[]{}" = gsfmterrorString evs pos (ds . ('\\':) . (ch:)) s
gsfmterrorString evs pos ds v@(GSConstr pos1 c [ GSRune ch, s ]) | c == gsvar ":" = gsfmterrorString evs pos (ds . (ch:)) s
gsfmterrorString evs pos ds v@(GSConstr pos1 c [ ch, s ]) | c == gsvar ":"  =
    gsfmterrorString evs pos (ds . ("§(<"++) . fmtPos $gshere . ("gsfmterrorString "++) . (gsvCode ch++) . (" : _ next"++) . (">)"++)) s
gsfmterrorString evs pos ds (GSConstr pos1 c []) | c == gsvar "nil" = gsfmterrorStringTerm ds
gsfmterrorString evs pos ds (GSConstr pos1 c _) =
    gsfmterrorStringTerm $ ds . ("§(<"++) . fmtPos $gshere . ("gsfmterrorString "++) . (fmtVarAtom c) . (" next"++) . (">)"++)
gsfmterrorString evs pos ds x = gsfmterrorStringTerm $ ds . ("§(<"++) . fmtPos $gshere . ("gsfmterrorString "++) . (gsvCode x++) . (" next"++) . (">)"++)

gsfmterrorStringExpr :: GSEvalState -> Pos -> (String -> String) -> GSValue -> IO (String -> String)
gsfmterrorStringExpr evs pos ds v = do
    ds1 <- gsfmterrorvalue evs{profCounter = Nothing} pos v
    gsfmterrorStringTerm $ ds . ("§("++) . ds1 . (')':)

gsfmterrorStringTerm :: (String -> String) -> IO (String -> String)
gsfmterrorStringTerm ds = return $ ds . ('}':)

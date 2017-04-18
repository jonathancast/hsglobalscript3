{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
module GSI.Functions (gslist, gslist_w, gsstring, gsstring_w, gsnatural, gsnatural_w, gsapiEvalString, gsapiEvalExternal, gsfmterrormsg) where

import Control.Exception (Exception(..), throwIO, try)

import Language.Haskell.TH.Lib (appE, varE)

import qualified Data.Map as Map

import GSI.Util (Pos, StackTrace(..), gshere, fmtPos)
import GSI.Syn (gsvar, fmtVarAtom)
import GSI.Error (GSException(..), fmtError)
import GSI.ThreadType (ThreadException(..))
import GSI.Value (GSValue(..), GSExternal(..), gsundefined_value, gsimplementationfailure, gsapply, gsfield, gsvCode)
import GSI.Eval (evalSync)
import API (apiImplementationFailure)

gslist = varE 'gslist_w `appE` gshere

gslist_w :: Pos -> [GSValue] -> GSValue
gslist_w pos [] = GSConstr pos (gsvar "nil") []
gslist_w pos (x:xn) = GSConstr pos (gsvar ":") [ x, gslist_w pos xn ]

gsstring = varE 'gsstring_w `appE` gshere

gsstring_w :: Pos -> String -> GSValue
gsstring_w pos s = gslist_w pos $ map GSRune s

gsnatural = varE 'gsnatural_w `appE` gshere

gsnatural_w :: Pos -> Integer -> GSValue
gsnatural_w pos n = GSNatural n

gsevalChar :: Pos -> GSValue -> IO Char
gsevalChar pos (GSRune ch) = return ch
gsevalChar pos v =
    throwIO $ GSExcImplementationFailure $gshere $ "gsevalChar " ++ gsvCode v ++ " next"

gsevalString :: Pos -> GSValue -> IO String
gsevalString pos v = gsevalString_w pos id v where
    gsevalString_w pos ds (GSConstr pos1 c [ chv, sv ]) | c == gsvar ":" = do
        ch <- gsevalChar pos chv
        gsevalString_w pos (ds . (ch:)) sv
    gsevalString_w pos ds (GSConstr pos1 c []) | c == gsvar "nil" = return $ ds ""
    gsevalString_w pos ds (GSConstr pos1 c as) =
        throwIO $ GSExcImplementationFailure $gshere $ "gsevalString " ++ fmtVarAtom c " next"
    gsevalString_w pos ds v =
        throwIO $ GSExcImplementationFailure $gshere $ "gsevalString " ++ gsvCode v ++ " next"

gsevalExternal :: GSExternal a => Pos -> GSValue -> IO a
gsevalExternal pos (GSThunk ts) = do
    v <- evalSync [StackTrace pos []] ts
    gsevalExternal pos v
gsevalExternal pos (GSError err) = throwGSError err
gsevalExternal pos v = throwIO $ GSExcImplementationFailure $gshere $ "gsevalExternal " ++ gsvCode v ++ " next"

gsapiEvalString :: Pos -> GSValue -> IO String
gsapiEvalString pos fnv = do
    mbs <- try $ gsevalString pos fnv
    case mbs of
        Right s -> return s
        Left (GSExcImplementationFailure pos1 err) -> throwIO $ TEImplementationFailure pos1 err
        Left (e :: GSException) -> $apiImplementationFailure $ "gsapiEvalString (gsevalString threw unknown exception " ++ displayException e ++ ") next"

gsapiEvalExternal :: GSExternal a => Pos -> GSValue -> IO a
gsapiEvalExternal pos v = do
    mbx <- try $ gsevalExternal pos v
    case mbx of
        Right x -> return x
        Left (GSExcImplementationFailure pos1 err) -> throwIO $ TEImplementationFailure pos1 err
        Left (e :: GSException) -> $apiImplementationFailure $ "gsapiEvalExternal (gsevalExternal threw unknown exception (" ++ show e ++ ")) next"

gsfmterrormsg = varE 'gsfmterrormsg_w `appE` gshere

gsfmterrormsg_w :: Pos -> GSValue -> IO String
gsfmterrormsg_w pos msg = do
    msgt <- $gsapply msg [ GSRecord $gshere (Map.fromList [
        (gsvar "paragraph-constituents", GSConstr $gshere (gsvar "nil") [])
      ]) ]
    msg_pcs <- $gsfield (gsvar "paragraph-constituents") msgt
    gsfmterrormsg_ww pos id msg_pcs

gsfmterrormsg_ww :: Pos -> (String -> String) -> GSValue -> IO String
gsfmterrormsg_ww pos ds (GSThunk th) = do
    v <- evalSync [StackTrace pos []] th
    gsfmterrormsg_ww pos ds v
gsfmterrormsg_ww pos ds (GSError err) = return $ (ds . ("<Error: "++) . (fmtError err++) . ('>':)) $ ""
gsfmterrormsg_ww pos0 ds (GSImplementationFailure pos1 msg) = return $ (ds . ("<Implementation Failure: "++) . (fmtPos pos1) . (msg++) . ('>':)) $ ""
gsfmterrormsg_ww pos0 ds (GSConstr pos1 c [ GSThunk pcth, msg1 ]) | c == gsvar ":" = do
    pcv <- evalSync [StackTrace pos0 []] pcth
    gsfmterrormsg_ww pos0 ds (GSConstr pos1 c [ pcv, msg1 ])
gsfmterrormsg_ww pos0 ds (GSConstr pos1 c [ GSError err, msg1 ]) | c == gsvar ":" =
    gsfmterrormsg_ww pos0 (ds . ("<Error: "++) . (fmtError err++) . ('>':)) msg1
gsfmterrormsg_ww pos0 ds (GSConstr pos1 c1 [ GSConstr pos2 c2 [ GSRune ch ], msg1 ]) | c1 == gsvar ":" && c2 == gsvar "char" =
    gsfmterrormsg_ww pos0 (ds . (ch:)) msg1
gsfmterrormsg_ww pos0 ds (GSConstr pos1 c1 [ GSConstr pos2 c2 [ ch ], msg1 ]) | c1 == gsvar ":" && c2 == gsvar "char" =
    gsfmterrormsg_ww pos0 (ds . ('<':) . fmtPos $gshere . ("gsfmterrormsg (char "++) . (gsvCode ch++) . (" : _"++) . (") next"++) . ('>':)) msg1
gsfmterrormsg_ww pos0 ds (GSConstr pos1 c1 [ GSConstr pos2 c2 [ x ], msg1 ]) | c1 == gsvar ":" && c2 == gsvar "gsv" = do
    xds <- gsfmterrorvalue pos0 x
    gsfmterrormsg_ww pos0 (ds . xds) msg1
gsfmterrormsg_ww pos0 ds (GSConstr pos1 c1 [ GSConstr pos2 c2 _, msg1 ]) | c1 == gsvar ":" =
    gsfmterrormsg_ww pos0 (ds . ('<':) . fmtPos $gshere . ("gsfmterrormsg ("++) . fmtVarAtom c2 . (" : _"++) . (") next"++) . ('>':)) msg1
gsfmterrormsg_ww pos0 ds (GSConstr pos1 c [ c0, msg1 ]) | c == gsvar ":" =
    gsfmterrormsg_ww pos0 (ds . ('<':) . fmtPos $gshere . ("gsfmterrormsg ("++) . (gsvCode c0++) . (" : _"++) . (") next"++) . ('>':)) msg1
gsfmterrormsg_ww pos0 ds (GSConstr pos1 c []) | c== gsvar "nil" = return $ ds $ ""
gsfmterrormsg_ww pos0 ds (GSConstr pos1 c as) =
    return $ (ds . ('<':) . fmtPos $gshere . ("gsfmterrormsg "++) . fmtVarAtom c . (" next"++) . ('>':)) $ ""
gsfmterrormsg_ww pos ds msg =
    return $ (ds . ('<':) . fmtPos $gshere . ("gsfmterrormsg "++) . (gsvCode msg++) . (" next"++) . ('>':)) $ ""

-- §begin§itemize
--     §item Evaluates thunks
--     §item Prints errors
-- §end
gsfmterrorvalue :: Pos -> GSValue -> IO (String -> String)
gsfmterrorvalue pos (GSThunk th) = do
    v <- evalSync [StackTrace pos []] th
    gsfmterrorvalue pos v
gsfmterrorvalue pos v@GSImplementationFailure{} = gsfmterrorvalueAtom pos v
gsfmterrorvalue pos v@GSError{} = gsfmterrorvalueAtom pos v
gsfmterrorvalue pos v@(GSConstr pos1 c [ GSRune{}, _ ]) | c == gsvar ":" = gsfmterrorvalueAtom pos v
gsfmterrorvalue pos (GSConstr pos1 c as) = foldl (\ s s' -> s . (' ':) . s') (fmtVarAtom c) <$> mapM (gsfmterrorvalueAtom pos) as
gsfmterrorvalue pos x = return $ ('<':) . fmtPos $gshere . ("gsfmterrorvalue "++) . (gsvCode x++) . (" next"++) . ('>':)

gsfmterrorvalueAtom :: Pos -> GSValue -> IO (String -> String)
gsfmterrorvalueAtom pos0 (GSImplementationFailure pos1 msg) = return $ ("<Implementation Failure: "++) . (fmtPos pos1) . (msg++) . ('>':)
gsfmterrorvalueAtom pos0 (GSError err) = return $ ('<':) . (fmtError err ++) . ('>':)
gsfmterrorvalueAtom pos0 (GSRune ch) | not (ch `elem` "\\/§()[]{}") = return $ ("r/"++) . (ch:) . ("/"++)
gsfmterrorvalueAtom pos v@(GSConstr pos1 c [ GSRune{}, _ ]) | c == gsvar ":" = gsfmterrorString pos ("qq{"++) v
gsfmterrorvalueAtom pos x = return $ ('<':) . fmtPos $gshere . ("gsfmterrorvalueAtom "++) . (gsvCode x++) . (" next"++) . ('>':)

gsfmterrorString :: Pos -> (String -> String) -> GSValue -> IO (String -> String)
gsfmterrorString pos ds (GSThunk th) = do
    v <- evalSync [StackTrace pos []] th
    gsfmterrorString pos ds v
gsfmterrorString pos ds v@(GSConstr pos1 c [ GSRune ch, s ]) | c == gsvar ":" && ch `elem` "\\§()[]{}" = gsfmterrorString pos (ds . ('\\':) . (ch:)) s
gsfmterrorString pos ds v@(GSConstr pos1 c [ GSRune ch, s ]) | c == gsvar ":" = gsfmterrorString pos (ds . (ch:)) s
gsfmterrorString pos ds v@(GSConstr pos1 c [ ch, s ]) | c == gsvar ":"  =
    gsfmterrorString pos (ds . ("§(<"++) . fmtPos $gshere . ("gsfmterrorString "++) . (gsvCode ch++) . (" : _ next"++) . (">)"++)) s
gsfmterrorString pos ds (GSConstr pos1 c []) | c == gsvar "nil" = gsfmterrorStringTerm ds
gsfmterrorString pos ds (GSConstr pos1 c _) =
    gsfmterrorStringTerm $ ds . ("§(<"++) . fmtPos $gshere . ("gsfmterrorString "++) . (fmtVarAtom c) . (" next"++) . (">)"++)
gsfmterrorString pos ds x = gsfmterrorStringTerm $ ds . ("§(<"++) . fmtPos $gshere . ("gsfmterrorString "++) . (gsvCode x++) . (" next"++) . (">)"++)

gsfmterrorStringExpr :: Pos -> (String -> String) -> GSValue -> IO (String -> String)
gsfmterrorStringExpr pos ds v = do
    ds1 <- gsfmterrorvalue pos v
    gsfmterrorStringTerm $ ds . ("§("++) . ds1 . (')':)

gsfmterrorStringTerm :: (String -> String) -> IO (String -> String)
gsfmterrorStringTerm ds = return $ ds . ('}':)

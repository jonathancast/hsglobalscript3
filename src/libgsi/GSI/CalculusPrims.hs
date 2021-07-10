{-# LANGUAGE TemplateHaskell #-}
module GSI.CalculusPrims (gsparand, gsmergeenv, gspriminsufficientcases) where

import qualified Data.Map as Map

import Control.Concurrent (readMVar)

import GSI.Util (Pos, StackTrace(..), gshere, fmtPos)
import GSI.RTS (awaitAny)
import GSI.Prof (newProfCounter)
import GSI.Syn (gsvar, fmtVarAtom)
import GSI.Value (GSValue(..), GSThunk(..), GSThunkState(..), GSError(..), GSEvalState(..), gsimplementationfailure, gsvCode, whichExternal, fmtExternal)
import GSI.Eval (GSResult(..), eval, evalSync, stCode)

gsparand :: GSEvalState -> Pos -> GSValue -> GSValue -> IO GSValue
gsparand evs pos (GSConstr pos1 cx []) _ | cx == gsvar "0" = return $ GSConstr pos (gsvar "0") []
gsparand evs pos _ (GSConstr pos1 cy []) | cy == gsvar "0" = return $ GSConstr pos (gsvar "0") []
gsparand evs pos (GSThunk xs) (GSThunk ys) = do
    (xv, yv) <- gspareval evs pos xs ys
    gsparand evs pos xv yv
gsparand evs pos (GSThunk xs) y = do
    xv <- evalSync (msgChannel evs) (profCounter evs) [StackTrace pos []] xs
    gsparand evs pos xv y
gsparand evs pos x (GSThunk ys) = do
    yv <- evalSync (msgChannel evs) (profCounter evs) [StackTrace pos []] ys
    gsparand evs pos x yv
gsparand evs pos x@GSImplementationFailure{} _ = return x
gsparand evs pos x@GSError{} _ = return x
gsparand evs pos _ y@GSImplementationFailure{} = return y
gsparand evs pos _ y@GSInvalidProgram{} = return y
gsparand evs pos _ y@GSError{} = return y
gsparand evs pos (GSConstr _ cx [ex]) (GSConstr _ cy [ey]) | cx == gsvar "1" && cy == gsvar "1" = do
    ez <- gsmergeenv evs pos ex ey
    case ez of
        GSImplementationFailure{} -> return ez
        GSRecord{} -> return $ GSConstr pos (gsvar "1") [ez]
        _ -> return $ $gsimplementationfailure $ "gsparand (1 _) (1 _) (gsmergeenv returned " ++ gsvCode ez ++ ") next"
gsparand evs pos x y = return $ $gsimplementationfailure $ "gsparand " ++ gsvCode x ++ ' ' : gsvCode y ++ " next"

gsmergeenv :: GSEvalState -> Pos -> GSValue -> GSValue -> IO GSValue
gsmergeenv evs pos (GSThunk xs) (GSThunk ys) = do
    (xv, yv) <- gspareval evs pos xs ys
    gsmergeenv evs pos xv yv
gsmergeenv evs pos (GSThunk xs) y = do
    xv <- evalSync (msgChannel evs) (profCounter evs) [StackTrace pos []] xs
    gsmergeenv evs pos xv y
gsmergeenv evs pos x (GSThunk ys) = do
    yv <- evalSync (msgChannel evs) (profCounter evs) [StackTrace pos []] ys
    gsmergeenv evs pos x yv
gsmergeenv evs pos ex@GSImplementationFailure{} ey = return ex
gsmergeenv evs pos ex ey@GSImplementationFailure{} = return ey
gsmergeenv evs pos ex@GSError{} ey = return ex
gsmergeenv evs pos (GSRecord _ ex) (GSRecord _ ey) = return $ GSRecord pos (Map.union ex ey)
gsmergeenv evs pos ex ey = return $ $gsimplementationfailure $ "gsmergeenv " ++ gsvCode ex ++ ' ' : gsvCode ey ++ " next"

gspareval :: GSEvalState -> Pos -> GSThunk -> GSThunk -> IO (GSValue, GSValue)
gspareval evs pos xs ys = do
    xpc <- maybe (return Nothing) (fmap Just . const newProfCounter) (profCounter evs)
    ypc <- maybe (return Nothing) (fmap Just . const newProfCounter) (profCounter evs)
    xr <- eval evs{profCounter = xpc} [StackTrace pos []] xs
    yr <- eval evs{profCounter = ypc} [StackTrace pos []] ys
    case (xr, yr) of
        (GSStack ex, GSStack ey) -> do
            awaitAny [ ex, ey ]
            return (GSThunk xs, GSThunk ys)
        (GSIndirection xv, GSIndirection yv) -> return (xv, yv)
        (_, GSIndirection yv) -> return (GSThunk xs, yv)
        (GSIndirection xv, _) -> return (xv, GSThunk ys)
        _ -> let err = $gsimplementationfailure $ "gspareval" ++ stCode xr ++ ' ' : stCode yr ++ " next" in return (err, err)

gspriminsufficientcases :: GSEvalState -> Pos -> GSValue -> IO GSValue
gspriminsufficientcases evs pos v@GSError{} = return v
gspriminsufficientcases evs pos v@GSImplementationFailure{} = return v
gspriminsufficientcases evs pos v@GSInvalidProgram{} = return v
gspriminsufficientcases evs pos (GSThunk th) = do
    v <- evalSync (msgChannel evs) Nothing [StackTrace pos []] th
    gspriminsufficientcases evs pos v
gspriminsufficientcases evs pos v@GSConstr{} = GSError . GSErrInsufficientCases pos . ($ "") <$> fmtValue v -- Buggy buggy should take imported-as names into account
gspriminsufficientcases evs pos v@GSRecord{} = GSError . GSErrInsufficientCases pos . ($ "") <$> fmtValue v -- Buggy buggy should take imported-as names into account
gspriminsufficientcases evs pos (GSExternal e) = GSError . GSErrInsufficientCases pos . ($ "") . (\ ds -> ('<':) . ds . ('>':)) <$> fmtExternal e
gspriminsufficientcases evs pos (GSRune ch) = return $ GSError $ GSErrInsufficientCases pos $ ("r/" ++ ch : "/")
gspriminsufficientcases evs pos e = return $ $gsimplementationfailure $ "gspriminsufficientcases " ++ gsvCode e ++ " next"

-- §begin§itemize
--     §item Doesn't evaluate thunks
--     §item Doesn't print errors (prints _ instead)
-- §end
fmtValue :: GSValue -> IO (String -> String)
fmtValue v@GSError{} = fmtValueAtom v
fmtValue v@GSRecord{} = fmtValueAtom v
fmtValue (GSConstr pos c xn) = foldl (\ s s' -> s . (' ':) . s') (fmtVarAtom c) <$> mapM fmtValueAtom xn
fmtValue (GSThunk t) = do
    ts <- readMVar t
    case ts of
        GSTSIndirection v -> fmtValue v
        _ -> return ('_':)
fmtValue v = return $ ('<':) . fmtPos $gshere . ("Unknown value: " ++) . (gsvCode v ++) . ('>':)

fmtValueAtom :: GSValue -> IO (String -> String)
fmtValueAtom GSError{} = return ('_':)
fmtValueAtom (GSConstr pos c []) = return (fmtVarAtom c)
fmtValueAtom v@GSConstr{} = fmtParens <$> fmtValue v
fmtValueAtom (GSRecord pos fs) = do
    dsfs <- mapM (\ (f, x) -> (\ dsx -> ('\'':) . fmtVarAtom f . (" ∝ "++) . dsx . ("; "++)) <$> fmtValue x) (Map.toList fs)
    case dsfs of
        [] -> return ("〈〉"++)
        _ -> return $ ("〈 "++) . foldr (.) id dsfs . ('〉':)
fmtValueAtom (GSThunk t) = do
    ts <- readMVar t
    case ts of
        GSTSIndirection v -> fmtValueAtom v
        _ -> return ('_':)
fmtValueAtom (GSRune r)
    | r `elem` "/\\§()[]{}" = return $ ("r/"++) . ('\\':) . (r:) . ('/':)
    | r == '\n' = return $ ("r/\\n/"++)
    | otherwise = return $ ("r/"++) . (r:) . ('/':)
fmtValueAtom (GSExternal e) = return $ ('<':) . (whichExternal e++) . ('>':)
fmtValueAtom v = return $ ('<':) . fmtPos $gshere . ("Unknown value: " ++) . (gsvCode v ++) . ('>':)

fmtParens :: (String -> String) -> String -> String
fmtParens s = ('(':) . s . (')':)

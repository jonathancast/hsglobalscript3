{-# LANGUAGE TemplateHaskell #-}
module GSI.CalculusPrims (gsparand, gsmergeenv, gspriminsufficientcases) where

import qualified Data.Map as Map

import Control.Concurrent (readMVar)

import GSI.Util (Pos, StackTrace(..), gshere, fmtPos)
import GSI.RTS (OPort, awaitAny)
import GSI.Error (GSError(..))
import GSI.Message (Message)
import GSI.Syn (gsvar, fmtVarAtom)
import GSI.Value (GSValue(..), GSThunk(..), GSThunkState(..), gsimplementationfailure, gsvCode, whichExternal, fmtExternal)
import GSI.Eval (GSResult(..), eval, evalSync, stCode)

gsparand :: OPort Message -> Pos -> GSValue -> GSValue -> IO GSValue
gsparand msg pos (GSConstr pos1 cx []) _ | cx == gsvar "0" = return $ GSConstr pos (gsvar "0") []
gsparand msg pos _ (GSConstr pos1 cy []) | cy == gsvar "0" = return $ GSConstr pos (gsvar "0") []
gsparand msg pos (GSThunk xs) (GSThunk ys) = do
    (xv, yv) <- gspareval msg pos xs ys
    gsparand msg pos xv yv
gsparand msg pos (GSThunk xs) y = do
    xv <- evalSync msg [StackTrace pos []] xs
    gsparand msg pos xv y
gsparand msg pos x (GSThunk ys) = do
    yv <- evalSync msg [StackTrace pos []] ys
    gsparand msg pos x yv
gsparand msg pos x@GSImplementationFailure{} _ = return x
gsparand msg pos x@GSError{} _ = return x
gsparand msg pos _ y@GSImplementationFailure{} = return y
gsparand msg pos _ y@GSInvalidProgram{} = return y
gsparand msg pos _ y@GSError{} = return y
gsparand msg pos (GSConstr _ cx [ex]) (GSConstr _ cy [ey]) | cx == gsvar "1" && cy == gsvar "1" = do
    ez <- gsmergeenv msg pos ex ey
    case ez of
        GSImplementationFailure{} -> return ez
        GSRecord{} -> return $ GSConstr pos (gsvar "1") [ez]
        _ -> return $ $gsimplementationfailure $ "gsparand (1 _) (1 _) (gsmergeenv returned " ++ gsvCode ez ++ ") next"
gsparand msg pos x y = return $ $gsimplementationfailure $ "gsparand " ++ gsvCode x ++ ' ' : gsvCode y ++ " next"

gsmergeenv :: OPort Message -> Pos -> GSValue -> GSValue -> IO GSValue
gsmergeenv msg pos (GSThunk xs) (GSThunk ys) = do
    (xv, yv) <- gspareval msg pos xs ys
    gsmergeenv msg pos xv yv
gsmergeenv msg pos (GSThunk xs) y = do
    xv <- evalSync msg [StackTrace pos []] xs
    gsmergeenv msg pos xv y
gsmergeenv msg pos x (GSThunk ys) = do
    yv <- evalSync msg [StackTrace pos []] ys
    gsmergeenv msg pos x yv
gsmergeenv msg pos ex@GSImplementationFailure{} ey = return ex
gsmergeenv msg pos ex ey@GSImplementationFailure{} = return ey
gsmergeenv msg pos ex@GSError{} ey = return ex
gsmergeenv msg pos (GSRecord _ ex) (GSRecord _ ey) = return $ GSRecord pos (Map.union ex ey)
gsmergeenv msg pos ex ey = return $ $gsimplementationfailure $ "gsmergeenv " ++ gsvCode ex ++ ' ' : gsvCode ey ++ " next"

gspareval :: OPort Message -> Pos -> GSThunk -> GSThunk -> IO (GSValue, GSValue)
gspareval msg pos xs ys = do
    xr <- eval msg [StackTrace pos []] xs
    yr <- eval msg [StackTrace pos []] ys
    case (xr, yr) of
        (GSStack ex, GSStack ey) -> do
            awaitAny [ ex, ey ]
            return (GSThunk xs, GSThunk ys)
        (GSIndirection xv, GSIndirection yv) -> return (xv, yv)
        (_, GSIndirection yv) -> return (GSThunk xs, yv)
        (GSIndirection xv, _) -> return (xv, GSThunk ys)
        _ -> let err = $gsimplementationfailure $ "gspareval" ++ stCode xr ++ ' ' : stCode yr ++ " next" in return (err, err)

gspriminsufficientcases :: OPort Message -> Pos -> GSValue -> IO GSValue
gspriminsufficientcases msg pos v@GSError{} = return v
gspriminsufficientcases msg pos v@GSImplementationFailure{} = return v
gspriminsufficientcases msg pos v@GSInvalidProgram{} = return v
gspriminsufficientcases msg pos (GSThunk th) = do
    v <- evalSync msg [StackTrace pos []] th
    gspriminsufficientcases msg pos v
gspriminsufficientcases msg pos v@GSConstr{} = GSError . GSErrInsufficientCases pos . ($ "") <$> fmtValue v -- Buggy buggy should take imported-as names into account
gspriminsufficientcases msg pos v@GSRecord{} = GSError . GSErrInsufficientCases pos . ($ "") <$> fmtValue v -- Buggy buggy should take imported-as names into account
gspriminsufficientcases msg pos (GSExternal e) = GSError . GSErrInsufficientCases pos . ($ "") . (\ ds -> ('<':) . ds . ('>':)) <$> fmtExternal e
gspriminsufficientcases msg pos (GSRune ch) = return $ GSError $ GSErrInsufficientCases pos $ ("r/" ++ ch : "/")
gspriminsufficientcases msg pos e = return $ $gsimplementationfailure $ "gspriminsufficientcases " ++ gsvCode e ++ " next"

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

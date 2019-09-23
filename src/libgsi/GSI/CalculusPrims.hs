{-# LANGUAGE TemplateHaskell #-}
module GSI.CalculusPrims (gsparand, gsmergeenv, gspriminsufficientcases) where

import qualified Data.Map as Map

import Control.Concurrent.MVar (readMVar)

import GSI.Util (Pos, StackTrace(..), gshere, fmtPos)
import GSI.RTS (awaitAny)
import GSI.Error (GSError(..))
import GSI.Syn (gsvar, fmtVarAtom)
import GSI.Value (GSValue(..), GSThunk(..), GSThunkState(..), gsimplementationfailure, gsvCode, whichExternal, fmtExternal)
import GSI.Eval (GSResult(..), eval, evalSync, stCode)

gsparand :: Pos -> GSValue -> GSValue -> IO GSValue
gsparand pos (GSConstr pos1 cx []) _ | cx == gsvar "0" = return $ GSConstr pos (gsvar "0") []
gsparand pos _ (GSConstr pos1 cy []) | cy == gsvar "0" = return $ GSConstr pos (gsvar "0") []
gsparand pos (GSThunk xs) (GSThunk ys) = do
    (xv, yv) <- gspareval pos xs ys
    gsparand pos xv yv
gsparand pos (GSThunk xs) y = do
    xv <- evalSync [StackTrace pos []] xs
    gsparand pos xv y
gsparand pos x (GSThunk ys) = do
    yv <- evalSync [StackTrace pos []] ys
    gsparand pos x yv
gsparand pos x@GSImplementationFailure{} _ = return x
gsparand pos _ y@GSImplementationFailure{} = return y
gsparand pos _ y@GSInvalidProgram{} = return y
gsparand pos _ y@GSError{} = return y
gsparand pos (GSConstr _ cx [ex]) (GSConstr _ cy [ey]) | cx == gsvar "1" && cy == gsvar "1" = do
    ez <- gsmergeenv pos ex ey
    case ez of
        GSImplementationFailure{} -> return ez
        GSRecord{} -> return $ GSConstr pos (gsvar "1") [ez]
        _ -> return $ $gsimplementationfailure $ "gsparand (1 _) (1 _) (gsmergeenv returned " ++ gsvCode ez ++ ") next"
gsparand pos x y = return $ $gsimplementationfailure $ "gsparand " ++ gsvCode x ++ ' ' : gsvCode y ++ " next"

gsmergeenv :: Pos -> GSValue -> GSValue -> IO GSValue
gsmergeenv pos (GSThunk xs) (GSThunk ys) = do
    (xv, yv) <- gspareval pos xs ys
    gsmergeenv pos xv yv
gsmergeenv pos (GSThunk xs) y = do
    xv <- evalSync [StackTrace pos []] xs
    gsmergeenv pos xv y
gsmergeenv pos x (GSThunk ys) = do
    yv <- evalSync [StackTrace pos []] ys
    gsmergeenv pos x yv
gsmergeenv pos ex@GSImplementationFailure{} ey = return ex
gsmergeenv pos ex ey@GSImplementationFailure{} = return ey
gsmergeenv pos ex@GSError{} ey = return ex
gsmergeenv pos (GSRecord _ ex) (GSRecord _ ey) = return $ GSRecord pos (Map.union ex ey)
gsmergeenv pos ex ey = return $ $gsimplementationfailure $ "gsmergeenv " ++ gsvCode ex ++ ' ' : gsvCode ey ++ " next"

gspareval :: Pos -> GSThunk -> GSThunk -> IO (GSValue, GSValue)
gspareval pos xs ys = do
    xr <- eval [StackTrace pos []] xs
    yr <- eval [StackTrace pos []] ys
    case (xr, yr) of
        (GSStack ex, GSStack ey) -> do
            awaitAny [ ex, ey ]
            return (GSThunk xs, GSThunk ys)
        (GSIndirection xv, GSIndirection yv) -> return (xv, yv)
        (_, GSIndirection yv) -> return (GSThunk xs, yv)
        (GSIndirection xv, _) -> return (xv, GSThunk ys)
        _ -> let err = $gsimplementationfailure $ "gspareval" ++ stCode xr ++ ' ' : stCode yr ++ " next" in return (err, err)

gspriminsufficientcases :: Pos -> GSValue -> IO GSValue
gspriminsufficientcases pos v@GSError{} = return v
gspriminsufficientcases pos v@GSImplementationFailure{} = return v
gspriminsufficientcases pos v@GSInvalidProgram{} = return v
gspriminsufficientcases pos (GSThunk th) = do
    v <- evalSync [StackTrace pos []] th
    gspriminsufficientcases pos v
gspriminsufficientcases pos v@GSConstr{} = GSError . GSErrInsufficientCases pos . ($ "") <$> fmtValue v -- Buggy buggy should take imported-as names into account
gspriminsufficientcases pos v@GSRecord{} = GSError . GSErrInsufficientCases pos . ($ "") <$> fmtValue v -- Buggy buggy should take imported-as names into account
gspriminsufficientcases pos (GSExternal e) = GSError . GSErrInsufficientCases pos . ($ "") . (\ ds -> ('<':) . ds . ('>':)) <$> fmtExternal e
gspriminsufficientcases pos e = return $ $gsimplementationfailure $ "gspriminsufficientcases " ++ gsvCode e ++ " next"

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

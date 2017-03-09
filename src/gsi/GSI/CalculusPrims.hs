{-# LANGUAGE TemplateHaskell #-}
module GSI.CalculusPrims (gsparand, gspriminsufficientcases) where

import qualified Data.Map as Map

import GSI.Util (Pos, StackTrace(..), gshere, fmtPos)
import GSI.RTS (awaitAny)
import GSI.Error (GSError(..))
import GSI.Syn (gsvar, fmtVarAtom)
import GSI.Value (GSValue(..), gsimplementationfailure, gsvCode)
import GSI.Eval (GSResult(..), eval, evalSync, stCode)

gsparand :: Pos -> GSValue -> GSValue -> IO GSValue
gsparand pos (GSConstr pos1 cx []) _ | cx == gsvar "0" = return $ $gsimplementationfailure $ "gsparand 0 _ next" -- > fail
gsparand pos _ (GSConstr pos1 cy []) | cy == gsvar "0" = return $ $gsimplementationfailure $ "gsparand _ 0 next" -- > fail
gsparand pos x@(GSThunk xs) y@(GSThunk ys) = do
    xr <- eval [StackTrace pos []] xs
    yr <- eval [StackTrace pos []] ys
    case (xr, yr) of
        (GSStack ex, GSStack ey) -> do
            awaitAny [ ex, ey ]
            gsparand pos x y
        (GSIndirection xv, GSIndirection yv) -> gsparand pos xv yv
        (_, GSIndirection yv) -> gsparand pos x yv
        _ -> return $ $gsimplementationfailure $ "gsparand " ++ stCode xr ++ ' ' : stCode yr ++ " next" -- eval both, wait for one, then loop
gsparand pos (GSThunk xs) y = do
    xv <- evalSync [StackTrace pos []] xs
    gsparand pos xv y
gsparand pos x (GSThunk ys) = do
    yv <- evalSync [StackTrace pos []] ys
    gsparand pos x yv
gsparand pos x@GSImplementationFailure{} _ = return x
gsparand pos _ y@GSImplementationFailure{} = return y
gsparand pos _ y@GSError{} = return y
gsparand pos (GSConstr posx cx [ex@GSImplementationFailure{}]) (GSConstr posy cy [ey]) | cx == gsvar "1" && cy == gsvar "1" = return ex
gsparand pos (GSConstr posx cx [ex]) (GSConstr posy cy [ey@GSImplementationFailure{}]) | cx == gsvar "1" && cy == gsvar "1" = return ey
gsparand pos (GSConstr posx cx [GSRecord _ ex]) (GSConstr posy cy [GSRecord _ ey]) | cx == gsvar "1" && cy == gsvar "1" =
    return $ GSConstr pos (gsvar "1") [GSRecord pos (Map.union ex ey)]
gsparand pos (GSConstr posx cx [ex]) (GSConstr posy cy [ey]) | cx == gsvar "1" && cy == gsvar "1" =
    return $ $gsimplementationfailure $ "gsparand (1 " ++ gsvCode ex ++ ") (1 " ++ gsvCode ey ++ ") next"
gsparand pos x y = return $ $gsimplementationfailure $ "gsparand " ++ gsvCode x ++ ' ' : gsvCode y ++ " next"

gspriminsufficientcases :: Pos -> GSValue -> IO GSValue
gspriminsufficientcases pos v@GSError{} = return v
gspriminsufficientcases pos v@GSImplementationFailure{} = return v
gspriminsufficientcases pos (GSThunk th) = do
    v <- evalSync [StackTrace pos []] th
    gspriminsufficientcases pos v
gspriminsufficientcases pos v@GSConstr{} = GSError . GSErrInsufficientCases pos . ($ "") <$> fmtValue v -- Buggy buggy should take imported-as names into account
gspriminsufficientcases pos e = return $ $gsimplementationfailure $ "gspriminsufficientcases " ++ gsvCode e ++ " next"

-- §begin§itemize
--     §item Doesn't evaluate thunks
--     §item Doesn't print errors (prints _ instead)
-- §end
fmtValue :: GSValue -> IO (String -> String)
fmtValue v@GSError{} = fmtValueAtom v
fmtValue (GSConstr pos c xn) = foldl (\ s s' -> s . (' ':) . s') (fmtVarAtom c) <$> mapM fmtValueAtom xn
fmtValue v = return $ ('<':) . fmtPos $gshere . ("Unknown value: " ++) . (gsvCode v ++) . ('>':)

fmtValueAtom :: GSValue -> IO (String -> String)
fmtValueAtom GSError{} = return ('_':)
fmtValueAtom v@GSConstr{} = fmtParens <$> fmtValue v
fmtValueAtom v = return $ ('<':) . fmtPos $gshere . ("Unknown value: " ++) . (gsvCode v ++) . ('>':)

fmtParens :: (String -> String) -> String -> String
fmtParens s = ('(':) . s . (')':)

{-# LANGUAGE TemplateHaskell #-}
module GSI.Prims (gsparand) where

import GSI.Util (Pos)
import GSI.RTS (awaitAny)
import GSI.Syn (gsvar, fmtVarAtom)
import GSI.Value (GSValue(..), gsimplementationFailure, gsvCode)
import GSI.Eval (GSResult(..), eval, evalSync, stCode)

gsparand :: Pos -> GSValue -> GSValue -> IO GSValue
gsparand pos (GSConstr pos1 cx []) _ | cx == gsvar "0" = return $ $gsimplementationFailure $ "gsparand 0 _ next" -- > fail
gsparand pos _ (GSConstr pos1 cy []) | cy == gsvar "0" = return $ $gsimplementationFailure $ "gsparand _ 0 next" -- > fail
gsparand pos x@(GSThunk xs) y@(GSThunk ys) = do
    xr <- eval xs
    yr <- eval ys
    case (xr, yr) of
        (GSStack ex, GSStack ey) -> do
            awaitAny [ ex, ey ]
            gsparand pos x y
        _ -> return $ $gsimplementationFailure $ "gsparand " ++ stCode xr ++ ' ' : stCode yr ++ " next" -- eval both, wait for one, then loop
gsparand pos (GSThunk xs) y = do
    xv <- evalSync xs
    gsparand pos xv y
gsparand pos x (GSThunk ys) = return $ $gsimplementationFailure $ "gsparand _ GSThunk next"
gsparand pos x@GSImplementationFailure{} _ = return x
gsparand pos _ y@GSImplementationFailure{} = return y
gsparand pos (GSConstr posx cx [ex]) (GSConstr posy cy [ey]) | cx == gsvar "1" && cy == gsvar "1" = return $ $gsimplementationFailure $ "gsparand 1 1 next"  -- > succeed
gsparand pos x y = return $ $gsimplementationFailure $ "gsparand " ++ gsvCode x ++ ' ' : gsvCode y ++ " next"

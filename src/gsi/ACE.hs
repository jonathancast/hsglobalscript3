{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ACE (aceEnter, aceEnterBCO, aceThrow) where

import GSI.Util (Pos)
import GSI.Value (GSValue(..), GSBCO(..), GSStackFrame(..), GSThunkState(..), gsimplementationFailure, gsvCode, bcoCode, gsstCode)
import {-# SOURCE #-} GSI.Eval (GSResult(..), evalSync)

aceEnter :: Pos -> GSValue -> [GSStackFrame] -> IO GSValue
aceEnter pos v@GSError{} st = return v
aceEnter pos v@GSImplementationFailure{} st = return v
aceEnter pos (GSThunk th) st = do
    v <- evalSync th
    aceEnter pos v st
aceEnter pos0 (GSClosure pos1 bco) st = aceEnterBCO pos0 bco st
aceEnter pos v@GSConstr{} [] = return v
aceEnter pos0 v@GSConstr{} (GSStackForce pos1 k:st) = aceEnterBCO pos1 (k v) st
aceEnter pos v@GSConstr{} (k:st) = return $ $gsimplementationFailure $ "aceEnter (constr; continuation is " ++ gsstCode k ++ ") next"
aceEnter pos e st = return $ $gsimplementationFailure $ "aceEnter (expr = " ++ gsvCode e ++") next"

aceEnterBCO :: Pos -> GSBCO -> [GSStackFrame] -> IO GSValue
aceEnterBCO pos (GSBCOExpr e) st = e st
aceEnterBCO pos bco@GSBCOFun{} [] = return $ GSClosure pos bco
aceEnterBCO pos0 (GSBCOFun f) (GSStackArg pos1 a:st) = aceEnterBCO pos0 (f a) st
aceEnterBCO pos (GSBCOFun f) (k:st) = return $ $gsimplementationFailure $ "aceEnterBCO (function; cont = " ++ gsstCode k ++ ") next"
aceEnterBCO pos bco@GSBCOImp{} [] = return $ GSClosure pos bco
aceEnterBCO pos (GSBCOImp a) (k:st) = return $ $gsimplementationFailure $ "aceEnterBCO (GSBCOImp; cont = " ++ gsstCode k ++ ") next"
aceEnterBCO pos bco st = return $ $gsimplementationFailure $ "aceEnterBCO (expr = " ++ bcoCode bco ++") next"

aceThrow :: GSValue -> [GSStackFrame] -> IO GSValue
aceThrow v st = return v

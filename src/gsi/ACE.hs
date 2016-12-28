{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ACE (aceEnter, aceEnterBCO, aceThrow) where

import GSI.Util (Pos)
import GSI.Value (GSValue(..), GSBCO(..), GSStackFrame(..), GSThunkState(..), gsimplementationfailure, gsvCode, bcoCode, gsstCode)
import {-# SOURCE #-} GSI.Eval (GSResult(..), evalSync)

aceEnter :: Pos -> GSValue -> [GSStackFrame] -> IO GSValue
aceEnter pos v@GSError{} st = return v
aceEnter pos v@GSImplementationFailure{} st = return v
aceEnter pos (GSThunk th) st = do
    v <- evalSync th
    aceEnter pos v st
aceEnter pos0 (GSClosure pos1 bco) st = aceEnterBCO pos0 bco st
aceEnter pos0 v@GSLambda{} [] = return v
aceEnter pos0 (GSLambda pos1 f) (GSStackArg pos2 a:st) = aceEnter pos0 (f a) st
aceEnter pos0 (GSLambda pos1 f) (k:st) = return $ $gsimplementationfailure $ "aceEnter (lambda; cont = " ++ gsstCode k ++ ") next"
aceEnter pos (GSRawExpr e) st = e st
aceEnter pos v@GSConstr{} [] = return v
aceEnter pos0 v@GSConstr{} (GSStackForce pos1 k:st) = aceEnterBCO pos1 (k v) st
aceEnter pos v@GSConstr{} (k:st) = return $ $gsimplementationfailure $ "aceEnter (constr; continuation is " ++ gsstCode k ++ ") next"
aceEnter pos e st = return $ $gsimplementationfailure $ "aceEnter (expr = " ++ gsvCode e ++") next"

aceEnterBCO :: Pos -> GSBCO -> [GSStackFrame] -> IO GSValue
aceEnterBCO pos (GSBCOExpr e) st = e st
aceEnterBCO pos bco@GSBCOImp{} [] = return $ GSClosure pos bco
aceEnterBCO pos (GSBCOImp a) (k:st) = return $ $gsimplementationfailure $ "aceEnterBCO (GSBCOImp; cont = " ++ gsstCode k ++ ") next"
aceEnterBCO pos0 (GSBCOVar pos1 v) st = aceEnter pos1 v st
aceEnterBCO pos bco st = return $ $gsimplementationfailure $ "aceEnterBCO (expr = " ++ bcoCode bco ++") next"

aceThrow :: GSValue -> [GSStackFrame] -> IO GSValue
aceThrow v st = return v

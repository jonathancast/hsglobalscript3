{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ACE (aceEnter, aceEnterBCO, aceThrow) where

import GSI.Util (Pos, StackTrace(..))
import GSI.Value (GSValue(..), GSBCO(..), GSStackFrame(..), GSThunkState(..), gsimplementationfailure, gsvCode, bcoCode, gsstCode)
import {-# SOURCE #-} GSI.Eval (GSResult(..), evalSync)

aceEnter :: [StackTrace] -> GSValue -> [GSStackFrame] -> IO GSValue
aceEnter cs v@GSError{} st = aceThrow v st
aceEnter cs v@GSImplementationFailure{} st = aceThrow v st
aceEnter cs (GSThunk th) st = do
    v <- evalSync th
    aceEnter cs v st
aceEnter cs v@GSLambda{} [] = return v
aceEnter _ (GSLambda pos1 f) (GSStackArg pos2 a:st) = aceEnter [ StackTrace pos1 [], StackTrace pos2 [] ] (f a) st
aceEnter cs (GSLambda pos1 f) (k:st) = return $ $gsimplementationfailure $ "aceEnter (lambda; cont = " ++ gsstCode k ++ ") next"
aceEnter cs (GSRawExpr e) st = e st cs
aceEnter cs v@GSImp{} [] = return v
aceEnter cs (GSImp pos1 a) (k:st) = return $ $gsimplementationfailure $ "aceEnter (imp; cont = " ++ gsstCode k ++ ") next"
aceEnter cs v@GSConstr{} [] = return v
aceEnter cs v@GSConstr{} (GSStackForce pos1 k:st) = aceEnterBCO pos1 (k v) st
aceEnter cs v@GSConstr{} (k:st) = return $ $gsimplementationfailure $ "aceEnter (constr; continuation is " ++ gsstCode k ++ ") next"
aceEnter cs e st = return $ $gsimplementationfailure $ "aceEnter (expr = " ++ gsvCode e ++") next"

aceEnterBCO :: Pos -> GSBCO -> [GSStackFrame] -> IO GSValue
aceEnterBCO pos (GSBCOExpr e) st = e st [StackTrace pos []]
aceEnterBCO pos0 (GSBCOVar pos1 v) st = aceEnter [ StackTrace pos1 [], StackTrace pos1 [] ] v st
aceEnterBCO pos bco st = return $ $gsimplementationfailure $ "aceEnterBCO (expr = " ++ bcoCode bco ++") next"

aceThrow :: GSValue -> [GSStackFrame] -> IO GSValue
aceThrow v (GSStackArg{}:st) = aceThrow v st
aceThrow v (k:st) = return $ $gsimplementationfailure $ "aceThrow (continuation is " ++ gsstCode k ++ ") next"
aceThrow v [] = return v

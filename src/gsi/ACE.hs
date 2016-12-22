{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ACE (aceEnter, aceEnterBCO, aceApply) where

import GSI.Util (Pos)
import GSI.Value (GSValue(..), GSBCO(..), GSStackFrame(..), GSThunkState(..), gsimplementationFailure, gsvCode, bcoCode, gsstCode)
import {-# SOURCE #-} GSI.Eval (GSResult(..), evalSync)

aceEnter :: Pos -> GSValue -> [GSStackFrame] -> IO GSValue
aceEnter pos v@GSError{} st = return v
aceEnter pos (GSThunk th) st = do
    v <- evalSync th
    aceEnter pos v st
aceEnter pos0 (GSClosure pos1 bco) st = aceEnterBCO pos0 bco st
aceEnter pos e st = return $ $gsimplementationFailure $ "aceEnter (expr = " ++ gsvCode e ++") next"

aceEnterBCO :: Pos -> GSBCO -> [GSStackFrame] -> IO GSValue
aceEnterBCO pos (GSBCOExpr e) st = do
    v <- e
    aceEnter pos v st
aceEnterBCO pos (GSBCOFun f) [] = return $ $gsimplementationFailure $ "aceEnterBCO (function; stack empty) next"
aceEnterBCO pos0 (GSBCOFun f) (GSStackArg pos1 a:st) = aceEnterBCO pos0 (f a) st
aceEnterBCO pos (GSBCOFun f) (k:st) = return $ $gsimplementationFailure $ "aceEnterBCO (function; cont = " ++ gsstCode k ++ ") next"
aceEnterBCO pos bco@GSBCOImp{} [] = return $ GSClosure pos bco
aceEnterBCO pos (GSBCOImp a) (k:st) = return $ $gsimplementationFailure $ "aceEnterBCO (GSBCOImp; cont = " ++ gsstCode k ++ ") next"
aceEnterBCO pos bco st = return $ $gsimplementationFailure $ "aceEnterBCO (expr = " ++ bcoCode bco ++") next"

aceApply :: Pos -> GSValue -> [GSValue] -> IO GSValue
aceApply pos fn@GSError{} args = return fn
aceApply pos fn@GSImplementationFailure{} args = return fn
aceApply pos0 (GSClosure pos1 bco) args = aceCall pos0 pos1 bco args
aceApply pos (GSThunk th) args = do
    v <- evalSync th
    aceApply pos v args
aceApply pos fn args = return $ $gsimplementationFailure $ "aceApply (function = " ++ gsvCode fn ++") next"

aceCall pos0 pos1 (GSBCOFun f) (arg:args) = aceCall pos0 pos1 (f arg) args
aceCall pos0 pos1 (GSBCOFun f) [] = return $ GSClosure pos0 (GSBCOFun f)
aceCall pos0 pos1 (GSBCOExpr e) args = do
    v <- e
    aceApply pos0 v args
aceCall pos0 pos1 (GSBCOImp a) [] = return $ GSClosure pos0 (GSBCOImp a)
aceCall pos0 pos1 (GSBCOImp a) args = return $ $gsimplementationFailure $ "aceCall: arguments to an imperative block statement?"
aceCall pos0 pos1 bco args = return $ $gsimplementationFailure $ "aceCall (function = " ++ bcoCode bco ++ ") next"

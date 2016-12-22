{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ACE (aceEval, aceApply) where

import GSI.Util (Pos)
import GSI.Value (GSValue(..), GSBCO(..), GSThunkState(..), gsimplementationFailure, gsvCode, bcoCode)
import {-# SOURCE #-} GSI.Eval (GSResult(..), evalSync, stCode)

aceApply :: Pos -> GSValue -> [GSValue] -> IO GSValue
aceApply pos fn@GSError{} args = return fn
aceApply pos fn@GSImplementationFailure{} args = return fn
aceApply pos0 (GSClosure pos1 bco) args = aceCall pos0 pos1 bco args
aceApply pos (GSThunk th) args = do
    v <- evalSync th
    aceApply pos v args
aceApply pos fn args = return $ $gsimplementationFailure $ "aceApply (function = " ++ gsvCode fn ++") next"

aceEval :: Pos -> GSBCO -> IO GSValue
aceEval pos bco = return $ $gsimplementationFailure $ "aceEval (" ++ bcoCode bco ++ ") next"

aceCall pos0 pos1 (GSBCOFun f) (arg:args) = aceCall pos0 pos1 (f arg) args
aceCall pos0 pos1 (GSBCOFun f) [] = return $ GSClosure pos0 (GSBCOFun f)
aceCall pos0 pos1 (GSBCOExpr e) args = do
    v <- e
    aceApply pos0 v args
aceCall pos0 pos1 (GSBCOImp a) [] = return $ GSClosure pos0 (GSBCOImp a)
aceCall pos0 pos1 (GSBCOImp a) args = return $ $gsimplementationFailure $ "aceCall: arguments to an imperative block statement?"
aceCall pos0 pos1 bco args = return $ $gsimplementationFailure $ "aceCall (function = " ++ bcoCode bco ++ ") next"

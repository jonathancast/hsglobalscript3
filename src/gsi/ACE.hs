{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ACE (aceApply, aceUpdate) where

import Control.Concurrent (MVar, modifyMVar)

import GSI.Util (Pos)
import GSI.RTS (wakeup)
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

aceCall pos0 pos1 (GSBCOFun f) (arg:args) = aceCall pos0 pos1 (f arg) args
aceCall pos0 pos1 (GSBCOFun f) [] = return $ $gsimplementationFailure $ "aceCall (function = GSBCOFun; no args) next"
aceCall pos0 pos1 (GSBCOExpr e) args = do
    v <- e
    aceApply pos0 v args
aceCall pos0 pos1 (GSBCOImp a) [] = return $ GSClosure pos0 (GSBCOImp a)
aceCall pos0 pos1 (GSBCOImp a) args = return $ $gsimplementationFailure $ "aceCall: arguments to an imperative block statement?"
aceCall pos0 pos1 bco args = return $ $gsimplementationFailure $ "aceCall (function = " ++ bcoCode bco ++ ") next"

aceUpdate mv v = do
    mbb <- modifyMVar mv $ \ s -> case s of
        GSTSStack b -> return (GSTSIndirection v, Just b)
        _ -> return (GSTSIndirection v, Nothing)
    maybe (return ()) wakeup mbb

{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ACE (aceApply, aceUpdate) where

import Control.Concurrent (MVar, modifyMVar)
import Control.Exception (SomeException, catch, displayException)

import GSI.RTS (wakeup)
import GSI.Value (GSValue(..), GSThunkState(..), gsimplementationFailure, gsvCode)
import GSI.ByteCode (GSBCO(..), bcoCode)
import {-# SOURCE #-} GSI.Eval (GSResult(..), evalSync, stCode)

aceApply pos fn@GSError{} args = return fn
aceApply pos0 (GSClosure pos1 bco) args = aceCall pos0 pos1 bco args
    `catch` \ (e :: SomeException) -> return $ $gsimplementationFailure $ "aceCall threw an exception: " ++ displayException e
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

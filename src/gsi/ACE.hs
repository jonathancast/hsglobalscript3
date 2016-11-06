{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ACE (aceApply, aceUpdate) where

import Control.Concurrent (MVar, modifyMVar)

import GSI.RTS (wakeup)
import GSI.Value (GSValue(..), GSThunkState(..), gsimplementationFailure, gsvCode)
import GSI.Result (GSResult(..), stCode)
import {-# SOURCE #-} GSI.Eval (evalSync)

aceApply pos fn@GSError{} args = return fn
aceApply pos fn args = return $ $gsimplementationFailure $ "aceApply (function = " ++ gsvCode fn ++") next"

aceUpdate mv v = do
    mbb <- modifyMVar mv $ \ s -> case s of
        GSTSStack b -> return (GSTSIndirection v, Just b)
        _ -> return (GSTSIndirection v, Nothing)
    maybe (return ()) wakeup mbb

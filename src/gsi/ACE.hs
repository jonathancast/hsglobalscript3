{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ACE (Stack(..), aceApply, aceUpdate, aceEnter) where

import Control.Monad (forM_)

import Control.Concurrent (MVar, modifyMVar)

import GSI.Util (gshere)
import GSI.RTS (wakeup)
import GSI.Value (GSValue(..), GSThunkState(..), gsimplementationFailure, gsvCode)
import GSI.Result (GSResult(..), stCode)
import {-# SOURCE #-} GSI.Eval (evalSync)

data Stack
  = StUpdate (MVar GSThunkState)

aceEnter pos fn@(GSError e) stack = aceThrow fn stack
aceEnter pos fn stack = aceUnimpl_w $gshere ("aceEnter (function = " ++ gsvCode fn ++ ") next") stack

aceApply pos fn@GSError{} args = return fn
aceApply pos fn args = return $ $gsimplementationFailure $ "aceApply (function = " ++ gsvCode fn ++") next"

aceUnimpl_w pos err = aceThrow (GSImplementationFailure pos err)

aceThrow err stack= forM_ stack $ \ cont -> case cont of
    StUpdate mv -> aceUpdate mv err
    _ -> return ()

aceUpdate mv v = do
    mbb <- modifyMVar mv $ \ s -> case s of
        GSTSStack b -> return (GSTSIndirection v, Just b)
        _ -> return (GSTSIndirection v, Nothing)
    maybe (return ()) wakeup mbb

stackCode :: Stack -> String
stackCode StUpdate{} = "StUpdate"

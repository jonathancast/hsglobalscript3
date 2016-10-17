{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
module GSI.Eval (eval) where

import Control.Concurrent (forkIO, modifyMVar)

import GSI.Util (gshere)
import GSI.Value (GSValue(..), GSThunkState(..), gsvCode, gstsCode)
import GSI.Result (GSError(..), GSResult(..), implementationFailure)

import ACE (Stack(..), aceEnter)

eval :: GSValue a -> IO (GSResult a)
eval (GSUndefined pos) = return $ GSError (GSErrUnimpl pos)
eval (GSThunk mv) = modifyMVar mv $ \ st -> case st of
    GSApply pos fn args -> do
        forkIO $ aceEnter pos fn [ StApp args, StUpdate mv ]
        return (GSTSStack, GSStack)
    _ -> return (st, $implementationFailure $ "eval (thunk: " ++ gstsCode st ++ ") next")
eval v = return $ $implementationFailure $ "eval " ++ gsvCode v ++ " next"

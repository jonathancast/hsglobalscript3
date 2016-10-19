{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
module GSI.Eval (eval, evalSync) where

import Control.Concurrent (forkIO, modifyMVar)

import GSI.Util (gshere)
import GSI.RTS (newEvent)
import GSI.Value (GSValue(..), GSThunkState(..), gsvCode, gstsCode)
import GSI.Result (GSError(..), GSResult(..), implementationFailure, stCode)

import ACE (Stack(..), aceEnter)

eval :: GSValue a -> IO (GSResult a)
eval (GSUndefined pos) = return $ GSError (GSErrUnimpl pos)
eval (GSThunk mv) = modifyMVar mv $ \ st -> case st of
    GSApply pos fn args -> do
        e <- newEvent
        forkIO $ aceEnter pos fn [ StApp args, StUpdate mv ]
        return (GSTSStack e, GSStack)
    _ -> return (st, $implementationFailure $ "eval (thunk: " ++ gstsCode st ++ ") next")
eval v = return $ $implementationFailure $ "eval " ++ gsvCode v ++ " next"

evalSync :: GSValue a -> IO (GSResult a)
evalSync v = do
    st <- eval v
    case st of
        GSError e -> return $ GSError e
        _ -> return $ $implementationFailure $ "evalSync " ++ stCode st ++ " next"

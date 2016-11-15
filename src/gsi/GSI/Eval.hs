{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
module GSI.Eval (eval, evalSync) where

import Control.Concurrent (MVar, forkIO, modifyMVar)

import GSI.Util (gshere)
import GSI.RTS (newEvent, await)
import GSI.Value (GSValue(..), GSThunkState(..), gsimplementationFailure, gsvCode, gstsCode)
import GSI.Result (GSError(..), GSResult(..), stCode)

import ACE (aceApply, aceUpdate)

eval :: MVar (GSThunkState) -> IO GSResult
eval mv = modifyMVar mv $ \ st -> case st of
    GSTSExpr expr -> do
        e <- newEvent
        forkIO $ expr >>= aceUpdate mv
        return (GSTSStack e, GSStack e)
    GSApply pos fn args -> do
        e <- newEvent
        forkIO $ aceApply pos fn args >>= aceUpdate mv
        return (GSTSStack e, GSStack e)
    GSTSIndirection v -> return (GSTSIndirection v, GSIndirection v)
    _ -> return (st, GSIndirection $ $gsimplementationFailure $ "eval (thunk: " ++ gstsCode st ++ ") next")

evalSync :: MVar (GSThunkState) -> IO GSValue
evalSync mv = do
    st <- eval mv
    case st of
        GSStack b -> await b *> evalSync mv
        GSIndirection v -> case v of
            GSImplementationFailure pos err -> return v
            GSError err -> return v
            _ -> return $ $gsimplementationFailure $ "evalSync (GSIndirection " ++ gsvCode v ++ ") next"
        _ -> return $ $gsimplementationFailure $ "evalSync " ++ stCode st ++ " next"

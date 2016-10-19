{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
module GSI.Eval (eval, evalSync) where

import Control.Concurrent (forkIO, modifyMVar)

import GSI.Util (gshere)
import GSI.RTS (newEvent, await)
import GSI.Value (GSValue(..), GSThunkState(..), gsvCode, gstsCode)
import GSI.Result (GSError(..), GSResult(..), implementationFailure, stCode)

import qualified GSI.Value as GSV
import qualified GSI.Result as GSR

import ACE (Stack(..), aceEnter)

eval :: GSValue a -> IO (GSResult a)
eval (GSUndefined pos) = return $ GSError (GSErrUnimpl pos)
eval (GSThunk mv) = modifyMVar mv $ \ st -> case st of
    GSApply pos fn args -> do
        e <- newEvent
        forkIO $ aceEnter pos fn [ StApp args, StUpdate mv ]
        return (GSTSStack e, GSStack e)
    GSTSIndirection v -> return (GSTSIndirection v, GSIndirection v)
    _ -> return (st, $implementationFailure $ "eval (thunk: " ++ gstsCode st ++ ") next")
eval (GSV.GSImplementationFailure pos err) = return $ GSR.GSImplementationFailure pos err
eval v = return $ $implementationFailure $ "eval " ++ gsvCode v ++ " next"

evalSync :: GSValue a -> IO (GSResult a)
evalSync v = do
    st <- eval v
    case st of
        GSError e -> return $ GSError e
        GSR.GSImplementationFailure pos e -> return $ GSR.GSImplementationFailure pos e
        GSStack b -> await b *> evalSync v
        GSIndirection v -> evalSync v
        _ -> return $ $implementationFailure $ "evalSync " ++ stCode st ++ " next"

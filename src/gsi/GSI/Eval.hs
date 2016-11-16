{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
module GSI.Eval (GSResult(..), eval, evalSync, stCode) where

import Control.Concurrent (MVar, forkIO, modifyMVar)

import GSI.Util (gshere)
import GSI.RTS (Event, newEvent, await)
import GSI.Value (GSValue(..), GSThunkState(..), GSError(..), gsimplementationFailure, gsvCode, gstsCode)

import ACE (aceApply, aceUpdate)

data GSResult
  = GSStack Event
  | GSIndirection GSValue
  | GSWHNF

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
            GSImplementationFailure{} -> return v
            GSError{} -> return v
            GSClosure{} -> return v
            _ -> return $ $gsimplementationFailure $ "evalSync (GSIndirection " ++ gsvCode v ++ ") next"
        _ -> return $ $gsimplementationFailure $ "evalSync " ++ stCode st ++ " next"

stCode :: GSResult -> String
stCode GSStack{} = "GSStack"
stCode GSIndirection{} = "GSIndirection"
stCode GSWHNF{} = "GSWHNF"

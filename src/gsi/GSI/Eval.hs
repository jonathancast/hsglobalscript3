{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
module GSI.Eval (GSResult(..), eval, evalSync, stCode) where

import Control.Concurrent (MVar, forkIO, modifyMVar)

import GSI.Util (StackTrace(..), gshere)
import GSI.RTS (Event, newEvent, wakeup, await)
import GSI.Error (GSError(..))
import GSI.Value (GSValue(..), GSStackFrame(..), GSThunkState(..), gsimplementationfailure, gsvCode, gstsCode)

import ACE (aceEnter)

data GSResult
  = GSStack Event
  | GSIndirection GSValue
  | GSWHNF

eval :: MVar (GSThunkState) -> IO GSResult
eval mv = modifyMVar mv $ \ st -> case st of
    GSTSExpr expr -> do
        e <- newEvent
        forkIO $ expr [] [] >>= updateThunk mv
        return (GSTSStack e, GSStack e)
    GSApply pos fn args -> do
        e <- newEvent
        forkIO $ aceEnter [ StackTrace pos [] ] fn (map (GSStackArg pos) args) >>= updateThunk mv
        return (GSTSStack e, GSStack e)
    GSTSIndirection v -> return (GSTSIndirection v, GSIndirection v)
    GSTSStack e -> return (GSTSStack e, GSStack e)
    _ -> return (st, GSIndirection $ $gsimplementationfailure $ "eval (thunk: " ++ gstsCode st ++ ") next")

updateThunk mv v = do
    mbb <- modifyMVar mv $ \ s -> case s of
        GSTSStack b -> return (GSTSIndirection v, Just b)
        _ -> return (GSTSIndirection v, Nothing)
    maybe (return ()) wakeup mbb

evalSync :: MVar (GSThunkState) -> IO GSValue
evalSync mv = do
    st <- eval mv
    case st of
        GSStack b -> await b *> evalSync mv
        GSIndirection v -> case v of
            GSImplementationFailure{} -> return v
            GSError{} -> return v
            GSLambda{} -> return v
            GSImp{} -> return v
            GSThunk th -> evalSync th
            GSConstr{} -> return v
            _ -> return $ $gsimplementationfailure $ "evalSync (GSIndirection " ++ gsvCode v ++ ") next"
        _ -> return $ $gsimplementationfailure $ "evalSync " ++ stCode st ++ " next"

stCode :: GSResult -> String
stCode GSStack{} = "GSStack"
stCode GSIndirection{} = "GSIndirection"
stCode GSWHNF{} = "GSWHNF"

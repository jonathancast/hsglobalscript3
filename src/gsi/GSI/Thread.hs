{-# LANGUAGE RecursiveDo, TemplateHaskell #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module GSI.Thread (createThread, execMainThread) where

import Control.Monad (join)

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, modifyMVar, modifyMVar_, putMVar, readMVar)
import Control.Exception (throw)

import GSI.Util (Pos, gsfatal, gshere)
import GSI.RTS (Event, newEvent, wakeup, await)
import GSI.Value (GSValue(..), gsvCode)
import GSI.Result (GSResult(..), GSError, GSException(..), stCode, throwGSerror)
import GSI.Eval (evalSync)

data Promise = Promise (MVar GSValue)

data Thread = Thread {
    state :: MVar ThreadState,
    wait :: Event
  }

data ThreadState
  = ThreadStateRunning
  | ThreadStateError GSError
  | ThreadStateImplementationFailure Pos String
  | ThreadStateUnimpl Pos String

threadStateCode :: ThreadState -> String
threadStateCode ThreadStateRunning{} = "ThreadStateRunning"
threadStateCode ThreadStateError{} = "ThreadStateError"
threadStateCode ThreadStateImplementationFailure{} = "ThreadStateImplementationFailure"
threadStateCode ThreadStateUnimpl{} = "ThreadStateUnimpl"

createThread :: GSValue -> IO Thread
createThread v = do
    rec
        w <- newEvent
        sv <- newMVar ThreadStateRunning
        p <- newEmptyMVar
        let t = Thread{
            state = sv,
            wait = w
          }
        tid <- forkIO $ runThread [(v, Promise p)] t
    return t

runThread :: [(GSValue, Promise)] -> Thread -> IO ()
runThread c t = do
    join $ state t `modifyMVar` \ st -> case st of
        ThreadStateRunning -> do
            case c of
                [] -> return (ThreadStateUnimpl $gshere $ "runThread (state is ThreadStateRunning; code is empty) next", finishThread t)
                (v, p) : c' -> do
                    return (ThreadStateRunning, execInstr v p c' t)
        _ -> return (ThreadStateUnimpl $gshere $ "runThread (state is " ++ threadStateCode st ++ ") next", finishThread t)

execInstr (GSImplementationFailure pos e) p c t = do
    state t `modifyMVar_` \ _ -> return $ ThreadStateImplementationFailure pos e
execInstr (GSError err) p c t = do
    state t `modifyMVar_` \ _ -> return $ ThreadStateError err
    finishThread t
execInstr (GSThunk th) p c t = do
    v <- evalSync th
    execInstr v p c t
execInstr v p c t = do
    state t `modifyMVar_` \ _ -> return $ ThreadStateUnimpl $gshere $ "runThread (state is ThreadStateRunning; code is non-empty; next statement is " ++ gsvCode v ++ ") next"
    finishThread t

finishThread t = do
    wakeup $ wait t
    return ()

execMainThread :: Thread -> IO ()
execMainThread t = do
    await $ wait t
    st <- readMVar $ state t
    case st of
        ThreadStateUnimpl pos err -> throw $ GSExcImplementationFailure pos err
        ThreadStateError err -> throwGSerror err
        ThreadStateImplementationFailure pos err -> throw $ GSExcImplementationFailure pos err
        _ -> $gsfatal $ "execMainThread (state is " ++ threadStateCode st ++ ") next"

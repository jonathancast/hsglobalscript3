{-# LANGUAGE RecursiveDo, TemplateHaskell #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module GSI.Thread (createThread, execMainThread) where

import Control.Monad (join)

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, modifyMVar, putMVar, readMVar)
import Control.Exception (throw)

import GSI.Util (Pos, gsfatal, gshere)
import GSI.Value (GSValue)
import GSI.Result (GSResult(..), GSError, GSException(..), stCode, throwGSerror)
import GSI.Eval (eval)

data Promise a = Promise (MVar (GSValue a))

data Thread a = Thread {
    state :: MVar ThreadState,
    code :: MVar [(GSValue a, Promise a)], -- Always take §hs{state} first!
    wait :: MVar ()
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

createThread :: GSValue a -> IO (Thread a)
createThread v = do
    rec
        w <- newEmptyMVar
        sv <- newMVar ThreadStateRunning
        p <- newEmptyMVar
        c <- newMVar [(v, Promise p)]
        let t = Thread{
            state = sv,
            code = c,
            wait = w
          }
        tid <- forkIO $ runThread t
    return t

runThread :: Thread a -> IO ()
runThread t = do
    join $ state t `modifyMVar` \ st -> case st of
        ThreadStateRunning -> do
            c <- readMVar $ code t
            case c of
                [] -> return (ThreadStateUnimpl $gshere $ "runThread (state is ThreadStateRunning; code is empty) next", finishThread t)
                (v, p) : c' -> do
                    r <- eval v
                    case r of
                        GSError e -> return (ThreadStateError e, finishThread t)
                        GSImplementationFailure pos e -> return (ThreadStateImplementationFailure pos e, finishThread t)
                        _ -> return (ThreadStateUnimpl $gshere $ "runThread (state is ThreadStateRunning; code is non-empty; eval returns " ++ stCode r ++ ") next", finishThread t)
        _ -> return (ThreadStateUnimpl $gshere $ "runThread (state is " ++ threadStateCode st ++ ") next", finishThread t)

finishThread t = do
    wait t `putMVar` ()
    return ()

execMainThread :: Thread a -> IO ()
execMainThread t = do
    readMVar $ wait t
    st <- readMVar $ state t
    case st of
        ThreadStateUnimpl pos err -> throw $ GSExcImplementationFailure pos err
        ThreadStateError err -> throwGSerror err
        ThreadStateImplementationFailure pos err -> throw $ GSExcImplementationFailure pos err
        _ -> $gsfatal $ "execMainThread (state is " ++ threadStateCode st ++ ") next"

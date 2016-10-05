{-# LANGUAGE RecursiveDo, TemplateHaskell #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module GSI.Thread (createThread, execMainThread) where

import Control.Monad (join)

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, modifyMVar, putMVar, readMVar)
import Control.Exception (throw)

import GSI.Util (Pos, gsfatal, gshere)
import GSI.Value (GSValue)
import GSI.Result (GSException(..))

data Thread a = Thread {
    state :: MVar ThreadState,
    wait :: MVar ()
  }

data ThreadState
  = ThreadStateRunning
  | ThreadStateUnimpl Pos String

threadStateCode :: ThreadState -> String
threadStateCode ThreadStateRunning{} = "ThreadStateRunning"
threadStateCode ThreadStateUnimpl{} = "ThreadStateUnimpl"

createThread :: GSValue a -> IO (Thread a)
createThread v = do
    rec
        w <- newEmptyMVar
        sv <- newMVar ThreadStateRunning
        let t = Thread{
            state = sv,
            wait = w
          }
        tid <- forkIO $ runThread t
    return t

runThread :: Thread a -> IO ()
runThread t = do
    join $ state t `modifyMVar` \ st -> case st of
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
        _ -> $gsfatal $ "execMainThread (state is " ++ threadStateCode st ++ ") next"

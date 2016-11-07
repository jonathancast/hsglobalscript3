{-# LANGUAGE RecursiveDo, TemplateHaskell #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module GSI.Thread (createThread, execMainThread) where

import Control.Monad (join)

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, modifyMVar, modifyMVar_, putMVar, readMVar)
import Control.Exception (throw)

import GSI.Util (Pos, gsfatal, gshere)
import GSI.Value (GSValue(..), gsvCode)
import GSI.Result (GSResult(..), GSError, GSException(..), stCode, throwGSerror)
import GSI.Eval (evalSync)

data Promise = Promise (MVar GSValue)

data Thread = Thread {
    state :: MVar ThreadState,
    code :: MVar [(GSValue, Promise)], -- Always take §hs{state} first!
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

createThread :: GSValue -> IO Thread
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

runThread :: Thread -> IO ()
runThread t = do
    join $ state t `modifyMVar` \ st -> case st of
        ThreadStateRunning -> do
            c <- readMVar $ code t
            case c of
                [] -> return (ThreadStateUnimpl $gshere $ "runThread (state is ThreadStateRunning; code is empty) next", finishThread t)
                (v, p) : c' -> do
                    code t `modifyMVar_` \ _ -> return c'
                    return (ThreadStateRunning, execInstr v p t)
        _ -> return (ThreadStateUnimpl $gshere $ "runThread (state is " ++ threadStateCode st ++ ") next", finishThread t)

execInstr (GSImplementationFailure pos e) p t = do
    state t `modifyMVar_` \ _ -> return $ ThreadStateImplementationFailure pos e
execInstr (GSError err) p t = do
    state t `modifyMVar_` \ _ -> return $ ThreadStateError err
    finishThread t
execInstr (GSThunk th) p t = do
    v <- evalSync th
    execInstr v p t
execInstr v p t = do
    state t `modifyMVar_` \ _ -> return $ ThreadStateUnimpl $gshere $ "runThread (state is ThreadStateRunning; code is non-empty; next statement is " ++ gsvCode v ++ ") next"
    finishThread t

finishThread t = do
    code t `modifyMVar_` \ _ -> return []
    wait t `putMVar` ()
    return ()

execMainThread :: Thread -> IO ()
execMainThread t = do
    readMVar $ wait t
    st <- readMVar $ state t
    case st of
        ThreadStateUnimpl pos err -> throw $ GSExcImplementationFailure pos err
        ThreadStateError err -> throwGSerror err
        ThreadStateImplementationFailure pos err -> throw $ GSExcImplementationFailure pos err
        _ -> $gsfatal $ "execMainThread (state is " ++ threadStateCode st ++ ") next"

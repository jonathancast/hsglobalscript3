{-# LANGUAGE ViewPatterns, Rank2Types, RecursiveDo, ScopedTypeVariables, TemplateHaskell #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module GSI.Thread (createThread, execMainThread, waitThread, withThreadData) where

import Control.Monad (join)

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, modifyMVar, modifyMVar_, putMVar, readMVar)
import Control.Exception (SomeException, Exception(..), throwIO, throw, try)

import GSI.Util (Pos, gsfatal, gshere)
import GSI.RTS (newEvent, wakeup, await)
import GSI.Error (GSError, GSException(..), throwGSError)
import GSI.Value (GSValue(..))
import GSI.Eval (GSResult(..), stCode)
import GSI.ThreadType (Thread(..), ThreadState(..), ThreadData(..), ThreadException(..), threadStateCode)
import API (apiCall)

data Promise = Promise (MVar GSValue)

createThread :: Pos -> ThreadData -> GSValue -> IO Thread
createThread pos d v = do
    rec
        w <- newEvent
        sv <- newMVar ThreadStateRunning
        let t = Thread{
            state = sv,
            threadData = d,
            wait = w
          }
        tid <- forkIO $ do
            mb <- try $ apiCall pos v t
            state t `modifyMVar_` \ _ -> case mb of
                Left (e :: SomeException) -> case fromException e of
                    Just (TEError err) -> return $ ThreadStateError err
                    Just (TEImplementationFailure pos err) -> return $ ThreadStateImplementationFailure pos err
                    _ -> return $ ThreadStateUnimpl $gshere $ "Thread execution threw unknown exception " ++ displayException e
                Right _ -> return $ ThreadStateUnimpl $gshere $ "Successful execution of a thread next"
            wakeup $ wait t
    return t

execMainThread :: Thread -> IO ()
execMainThread t = do
    await $ wait t
    st <- readMVar $ state t
    case st of
        ThreadStateUnimpl pos err -> throwIO $ GSExcImplementationFailure pos err
        ThreadStateError err -> throwGSError err
        ThreadStateImplementationFailure pos err -> throwIO $ GSExcImplementationFailure pos err
        _ -> $gsfatal $ "execMainThread (state is " ++ threadStateCode st ++ ") next"

waitThread :: Thread -> IO ThreadState
waitThread t = do
    await $ wait t
    readMVar $ state t

withThreadData :: Thread -> (ThreadData -> a) -> a
withThreadData (Thread{threadData = d}) k = k d

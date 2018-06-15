{-# LANGUAGE ViewPatterns, Rank2Types, RecursiveDo, ScopedTypeVariables, TemplateHaskell #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module GSI.Thread (createThread, execMainThread, waitThread, withThreadData, createPromise, readPromise) where

import Control.Monad (join)

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, modifyMVar, modifyMVar_, putMVar, readMVar)
import Control.Exception (SomeException, Exception(..), throwIO, throw, try)

import GSI.Util (Pos, gsfatal, gshere)
import GSI.RTS (newEvent, wakeup, await)
import GSI.Error (GSError, GSException(..), throwGSInvalidProgram, throwGSError)
import GSI.Value (GSValue(..))
import GSI.Eval (GSResult(..), stCode)
import GSI.ThreadType (Thread(..), ThreadState(..), ThreadData(..), ThreadException(..), threadStateCode)
import API (apiCall)

data Promise = Promise (MVar GSValue)

createThread :: Pos -> ThreadData -> GSValue -> Maybe Promise -> IO Thread
createThread pos d v mbp = do
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
                    Just (TEInvalidProgram err) -> return $ ThreadStateInvalidProgram err
                    Just (TEImplementationFailure pos err) -> return $ ThreadStateImplementationFailure pos err
                    _ -> return $ ThreadStateUnimpl $gshere $ "Thread execution threw unknown exception " ++ displayException e
                Right v -> do
                    maybe (return ()) (`updatePromise` v) mbp
                    return ThreadStateSuccess
            wakeup $ wait t
    return t

execMainThread :: Thread -> IO ()
execMainThread t = do
    st <- waitThread t
    case st of
        ThreadStateUnimpl pos err -> throwIO $ GSExcImplementationFailure pos err
        ThreadStateInvalidProgram err -> throwGSInvalidProgram err
        ThreadStateError err -> throwGSError err
        ThreadStateImplementationFailure pos err -> throwIO $ GSExcImplementationFailure pos err
        ThreadStateSuccess -> return ()
        _ -> $gsfatal $ "execMainThread (state is " ++ threadStateCode st ++ ") next"

waitThread :: Thread -> IO ThreadState
waitThread t = do
    await $ wait t
    readMVar $ state t

withThreadData :: Thread -> (ThreadData -> a) -> a
withThreadData (Thread{threadData = d}) k = k d

createPromise :: IO Promise
createPromise = Promise <$> newEmptyMVar

updatePromise :: Promise -> GSValue -> IO ()
updatePromise (Promise mv) v = putMVar mv v

readPromise :: Promise -> IO GSValue
readPromise (Promise mv) = readMVar mv

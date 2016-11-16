{-# LANGUAGE ViewPatterns, RecursiveDo, ScopedTypeVariables, TemplateHaskell #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module GSI.Thread (Thread, createThread, execMainThread) where

import Control.Monad (join)

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, modifyMVar, modifyMVar_, putMVar, readMVar)
import Control.Exception (SomeException, Exception(..), throwIO, try)

import GSI.Util (Pos, gsfatal, gshere)
import GSI.RTS (newEvent, wakeup, await)
import GSI.Error (GSError, GSException(..), throwGSerror)
import GSI.Value (GSValue(..), GSBCO(..))
import GSI.Eval (GSResult(..), stCode)
import GSI.ThreadType (Thread(..), ThreadState(..), ThreadException(..), threadStateCode)
import API (apiCall)

data Promise = Promise (MVar GSValue)

createThread :: Pos -> GSValue -> IO Thread
createThread pos v = do
    rec
        w <- newEvent
        sv <- newMVar ThreadStateRunning
        let t = Thread{
            state = sv,
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
        ThreadStateError err -> throwGSerror err
        ThreadStateImplementationFailure pos err -> throwIO $ GSExcImplementationFailure pos err
        _ -> $gsfatal $ "execMainThread (state is " ++ threadStateCode st ++ ") next"

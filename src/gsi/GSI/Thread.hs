{-# LANGUAGE ViewPatterns, RecursiveDo, ScopedTypeVariables, TemplateHaskell #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module GSI.Thread (Thread, createThread, execMainThread) where

import Control.Monad (join)

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, modifyMVar, modifyMVar_, putMVar, readMVar)
import Control.Exception (SomeException, Exception(..), throwIO, try)

import GSI.Util (Pos, gsfatal, gshere, fmtPos)
import GSI.RTS (newEvent, wakeup, await)
import GSI.Error (GSError, GSException(..), throwGSerror, fmtError)
import GSI.Value (GSValue(..), gsvCode)
import GSI.Eval (GSResult(..), evalSync, stCode)
import GSI.ThreadType (Thread(..), ThreadState(..), threadStateCode)
import GSI.ByteCode (GSBCO(..), bcoCode)

data Promise = Promise (MVar GSValue)

createThread :: GSValue -> IO Thread
createThread v = do
    rec
        w <- newEvent
        sv <- newMVar ThreadStateRunning
        let t = Thread{
            state = sv,
            wait = w
          }
        tid <- forkIO $ do
            mb <- try $ execInstr v t
            state t `modifyMVar_` \ _ -> case mb of
                Left (e :: SomeException) -> case fromException e of
                    Just (TEError err) -> return $ ThreadStateError err
                    Just (TEImplementationFailure pos err) -> return $ ThreadStateImplementationFailure pos err
                    _ -> return $ ThreadStateUnimpl $gshere $ "Thread execution threw unknown exception " ++ displayException e
                Right _ -> return $ ThreadStateUnimpl $gshere $ "Successful execution of a thread next"
            wakeup $ wait t
    return t

data ThreadException
  = TEImplementationFailure Pos String
  | TEError GSError
  deriving Show

instance Exception ThreadException where
    displayException (TEImplementationFailure pos err) = fmtPos pos err
    displayException (TEError err) = fmtError err

execInstr (GSImplementationFailure pos e) t = throwIO $ TEImplementationFailure pos e
execInstr (GSError err) t = throwIO $ TEError err
execInstr (GSThunk th) t = do
    v <- evalSync th
    execInstr v t
execInstr (GSClosure pos bco) t = case bco of
    GSBCOImp a -> a t
    _ -> throwIO $ TEImplementationFailure $gshere $ "runThread (state is ThreadStateRunning; code is non-empty; next statement is " ++ bcoCode bco ++ ") next"
execInstr v t = do
    throwIO $ TEImplementationFailure $gshere $ "runThread (state is ThreadStateRunning; code is non-empty; next statement is " ++ gsvCode v ++ ") next"

execMainThread :: Thread -> IO ()
execMainThread t = do
    await $ wait t
    st <- readMVar $ state t
    case st of
        ThreadStateUnimpl pos err -> throwIO $ GSExcImplementationFailure pos err
        ThreadStateError err -> throwGSerror err
        ThreadStateImplementationFailure pos err -> throwIO $ GSExcImplementationFailure pos err
        _ -> $gsfatal $ "execMainThread (state is " ++ threadStateCode st ++ ") next"

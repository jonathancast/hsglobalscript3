{-# LANGUAGE BangPatterns #-}
module GSI.Prof (ProfCounter, prof, newProfCounter) where

import Data.Time.Clock (UTCTime, NominalDiffTime, getCurrentTime, addUTCTime)

import Control.Monad (join)

import Control.Concurrent (MVar, newMVar, modifyMVar)

import GSI.Util (StackTrace)
import GSI.Message (Message(..))
import GSI.RTS (OPort, writeOPort)

useProfiling = True

type ProfCounter = MVar (UTCTime, Int)

increment :: NominalDiffTime
increment = 0.001

prof :: Maybe ProfCounter -> OPort Message -> StackTrace -> IO ()
prof _ _ _ | not useProfiling = return ()
prof Nothing _ _ = return ()
prof (Just mv) msg st = join $ modifyMVar mv $ \ (ts, n) -> if n > 1000
    then do
        t <- getCurrentTime
        if t >= ts
            then return ((addUTCTime increment t, 0), writeOPort msg (MsgProfile st))
            else return ((ts, 0), return ())
    else let !n1 = n + 1 in return ((ts, n1), return ())

newProfCounter :: IO ProfCounter
newProfCounter = do
    t <- getCurrentTime
    newMVar (addUTCTime increment t, 0)

module GSI.Prof (ProfCounter, newProfCounter) where

import Data.Time.Clock (UTCTime, NominalDiffTime, getCurrentTime, addUTCTime)

import Control.Concurrent (MVar, newMVar)

type ProfCounter = MVar (UTCTime, Int)

increment :: NominalDiffTime
increment = 0.001

newProfCounter :: IO ProfCounter
newProfCounter = do
    t <- getCurrentTime
    newMVar (addUTCTime increment t, 0)

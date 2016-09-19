{-# LANGUAGE RecursiveDo, TemplateHaskell #-}

module GSI.Thread (createThread) where

import Control.Concurrent(forkIO)

import GSI.Util (gsfatal)
import GSI.Value (GSValue)

data Thread a = Thread {
  }

createThread :: GSValue a -> IO (Thread a)
createThread v = do
    rec
        let t = Thread{}
        tid <- forkIO $ runThread t
    return t

runThread :: Thread a -> IO ()
runThread t = return ()

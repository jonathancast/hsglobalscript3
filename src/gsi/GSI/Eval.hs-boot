module GSI.Eval (evalSync) where

import Control.Concurrent (MVar)

import GSI.Value (GSThunkState)
import GSI.Result (GSResult)

evalSync :: MVar (GSThunkState) -> IO GSResult

module GSI.Eval (evalSync) where

import Control.Concurrent (MVar)

import GSI.Value (GSValue, GSThunkState)

evalSync :: MVar (GSThunkState) -> IO GSValue

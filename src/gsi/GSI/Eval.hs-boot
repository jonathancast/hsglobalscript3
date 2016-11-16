module GSI.Eval (GSResult(..), evalSync, stCode) where

import Control.Concurrent (MVar)

import GSI.RTS (Event)
import GSI.Value (GSValue, GSThunkState)

data GSResult
  = GSStack Event
  | GSIndirection GSValue
  | GSWHNF

evalSync :: MVar (GSThunkState) -> IO GSValue

stCode :: GSResult -> String

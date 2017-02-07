module GSI.Eval (GSResult(..), evalSync, stCode) where

import Control.Concurrent (MVar)

import GSI.Util (StackTrace)
import GSI.RTS (Event)
import GSI.Value (GSValue, GSThunkState)

data GSResult
  = GSStack Event
  | GSIndirection GSValue
  | GSWHNF

evalSync :: [StackTrace] -> MVar (GSThunkState) -> IO GSValue

stCode :: GSResult -> String

module GSI.Eval (evalSync) where

import GSI.Value (GSValue)
import GSI.Result (GSResult)

evalSync :: GSValue a -> IO (GSResult a)

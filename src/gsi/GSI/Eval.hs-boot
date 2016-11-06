module GSI.Eval (evalSync) where

import GSI.Value (GSValue)
import GSI.Result (GSResult)

evalSync :: GSValue -> IO (GSResult a)

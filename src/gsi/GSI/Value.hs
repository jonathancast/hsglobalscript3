module GSI.Value (GSValue(..), gsvCode) where

import GSI.Util (Pos)

data GSValue a
  = GSUndefined Pos

gsvCode :: GSValue a -> String
gsvCode GSUndefined{} = "GSUndefined"

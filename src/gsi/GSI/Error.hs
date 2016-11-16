module GSI.Error (GSError(..)) where

import GSI.Util (Pos)

data GSError = GSErrUnimpl Pos
  deriving (Show)

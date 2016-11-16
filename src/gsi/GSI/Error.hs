module GSI.Error (GSError(..), fmtError) where

import GSI.Util (Pos, fmtPos)

data GSError = GSErrUnimpl Pos
  deriving (Show)

fmtError :: GSError -> String
fmtError (GSErrUnimpl pos) = fmtPos pos "Undefined"

module GSI.Error (GSError(..), GSException(..), fmtError) where

import Data.Typeable (Typeable)

import Control.Exception (Exception(..))

import GSI.Util (Pos, fmtPos)

data GSError = GSErrUnimpl Pos
  deriving (Show)

data GSException
  = GSExcUndefined Pos
  | GSExcImplementationFailure Pos String
  deriving (Typeable, Show)
instance Exception GSException where
    displayException (GSExcUndefined pos) = fmtPos pos "undefined"
    displayException (GSExcImplementationFailure pos err) = fmtPos pos err

fmtError :: GSError -> String
fmtError (GSErrUnimpl pos) = fmtPos pos "Undefined"

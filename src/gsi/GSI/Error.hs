{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module GSI.Error (GSError(..), GSException(..), throwGSerror, fmtError) where

import Data.Typeable (Typeable)

import Control.Exception (Exception(..), throw)

import GSI.Util (Pos, fmtPos, gshere)

data GSError = GSErrUnimpl Pos
  deriving (Show)

data GSException
  = GSExcUndefined Pos
  | GSExcImplementationFailure Pos String
  deriving (Typeable, Show)
instance Exception GSException where
    displayException (GSExcUndefined pos) = fmtPos pos "undefined"
    displayException (GSExcImplementationFailure pos err) = fmtPos pos err

throwGSerror (GSErrUnimpl pos) = throw $ GSExcUndefined pos
throwGSerror err = throw $ GSExcImplementationFailure $gshere $ "throwGSerror (" ++ show err ++ ") next"

fmtError :: GSError -> String
fmtError (GSErrUnimpl pos) = fmtPos pos "Undefined"

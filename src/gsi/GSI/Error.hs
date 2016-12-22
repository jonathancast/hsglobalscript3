{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module GSI.Error (GSError(..), GSException(..), throwGSError, fmtError) where

import Data.Typeable (Typeable)

import Control.Exception (Exception(..), throw)

import GSI.Util (Pos, fmtPos, gshere)

data GSError
  = GSErrUnimpl Pos
  | GSErrInsufficientCases Pos String
  deriving (Show)

data GSException
  = GSExcUndefined Pos
  | GSExcInsufficientCases Pos String
  | GSExcImplementationFailure Pos String
  deriving (Typeable, Show)
instance Exception GSException where
    displayException (GSExcUndefined pos) = fmtPos pos "undefined"
    displayException (GSExcInsufficientCases pos err) = fmtPos pos $ "Missing case: " ++ err
    displayException (GSExcImplementationFailure pos err) = fmtPos pos err

throwGSError (GSErrUnimpl pos) = throw $ GSExcUndefined pos
throwGSError (GSErrInsufficientCases pos err) = throw $ GSExcInsufficientCases pos err
throwGSError err = throw $ GSExcImplementationFailure $gshere $ "throwGSerror (" ++ show err ++ ") next"

fmtError :: GSError -> String
fmtError (GSErrUnimpl pos) = fmtPos pos "Undefined"
fmtError (GSErrInsufficientCases pos err) = fmtPos pos $ "Missing case: " ++ err

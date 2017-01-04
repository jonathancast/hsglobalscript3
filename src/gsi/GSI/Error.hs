{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module GSI.Error (GSError(..), GSException(..), throwGSError, fmtError) where

import Data.Typeable (Typeable)

import Control.Exception (Exception(..), throw)

import GSI.Util (Pos, StackTrace(..), fmtPos, fmtStackTrace, gshere)

data GSError
  = GSErrUnimpl StackTrace
  | GSErrInsufficientCases Pos String
  deriving (Show)

data GSException
  = GSExcUndefined StackTrace
  | GSExcInsufficientCases Pos String
  | GSExcImplementationFailure Pos String
  deriving (Typeable, Show)
instance Exception GSException where
    displayException (GSExcUndefined st) = fmtStackTrace st "undefined"
    displayException (GSExcInsufficientCases pos err) = fmtPos pos $ "Missing case: " ++ err
    displayException (GSExcImplementationFailure pos err) = fmtPos pos err

throwGSError (GSErrUnimpl st) = throw $ GSExcUndefined st
throwGSError (GSErrInsufficientCases pos err) = throw $ GSExcInsufficientCases pos err
throwGSError err = throw $ GSExcImplementationFailure $gshere $ "throwGSerror (" ++ show err ++ ") next"

fmtError :: GSError -> String
fmtError (GSErrUnimpl st) = fmtStackTrace st "Undefined"
fmtError (GSErrInsufficientCases pos err) = fmtPos pos $ "Missing case: " ++ err

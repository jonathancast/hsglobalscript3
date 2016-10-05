{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module GSI.Result (GSError(..), GSResult(..), GSException(..), implementationFailure, stCode) where

import Control.Exception (Exception(..))

import Data.Typeable (Typeable)

import Language.Haskell.TH.Syntax (location)
import Language.Haskell.TH.Lib (appE, conE)

import GSI.Util (Pos, fmtPos, gshere)

data GSResult a
  = GSImplementationFailure Pos String
  | GSError GSError

data GSError = GSErrUnimpl Pos

data GSException
  = GSExcUndefined Pos
  | GSExcImplementationFailure Pos String
  deriving (Typeable, Show)
instance Exception GSException where
    displayException (GSExcUndefined pos) = fmtPos pos "undefined"
    displayException (GSExcImplementationFailure pos err) = fmtPos pos err

stCode :: GSResult a -> String
stCode GSImplementationFailure{} = "GSImplementationFailure"
stCode GSError{} = "GSError"

implementationFailure = conE 'GSImplementationFailure `appE` gshere

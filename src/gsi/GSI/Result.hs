{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module GSI.Result (GSError(..), GSResult(..), GSException(..), implementationFailure, stCode, throwGSerror) where

import Control.Exception (Exception(..), throw)

import Data.Typeable (Typeable)

import Language.Haskell.TH.Syntax (location)
import Language.Haskell.TH.Lib (appE, conE)

import GSI.Util (Pos, fmtPos, gshere)
import GSI.RTS (Event)
import GSI.Value (GSValue)

data GSResult a
  = GSImplementationFailure Pos String
  | GSError GSError
  | GSStack Event
  | GSIndirection (GSValue a)

data GSError = GSErrUnimpl Pos
  deriving (Show)

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
stCode GSStack{} = "GSStack"
stCode GSIndirection{} = "GSIndirection"

throwGSerror (GSErrUnimpl pos) = throw $ GSExcUndefined pos
throwGSerror err = throw $ GSExcImplementationFailure $gshere $ "throwGSerror (" ++ show err ++ ") next"

implementationFailure = conE 'GSImplementationFailure `appE` gshere

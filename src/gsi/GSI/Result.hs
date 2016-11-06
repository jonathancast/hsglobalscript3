{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module GSI.Result (GSError(..), GSResult(..), GSException(..), stCode, throwGSerror) where

import Control.Exception (Exception(..), throw)

import Data.Typeable (Typeable)

import Language.Haskell.TH.Syntax (location)
import Language.Haskell.TH.Lib (appE, conE)

import GSI.Util (Pos, fmtPos, gshere)
import GSI.RTS (Event)
import GSI.Value (GSValue, GSError(..))

data GSResult
  = GSStack Event
  | GSIndirection GSValue
  | GSWHNF

data GSException
  = GSExcUndefined Pos
  | GSExcImplementationFailure Pos String
  deriving (Typeable, Show)
instance Exception GSException where
    displayException (GSExcUndefined pos) = fmtPos pos "undefined"
    displayException (GSExcImplementationFailure pos err) = fmtPos pos err

stCode :: GSResult -> String
stCode GSStack{} = "GSStack"
stCode GSIndirection{} = "GSIndirection"
stCode GSWHNF{} = "GSWHNF"

throwGSerror (GSErrUnimpl pos) = throw $ GSExcUndefined pos
throwGSerror err = throw $ GSExcImplementationFailure $gshere $ "throwGSerror (" ++ show err ++ ") next"

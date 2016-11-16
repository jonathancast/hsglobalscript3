{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module GSI.Result (GSException(..), throwGSerror) where

import Control.Exception (Exception(..), throw)

import Data.Typeable (Typeable)

import GSI.Util (Pos, fmtPos, gshere)
import GSI.Value (GSError(..))

data GSException
  = GSExcUndefined Pos
  | GSExcImplementationFailure Pos String
  deriving (Typeable, Show)
instance Exception GSException where
    displayException (GSExcUndefined pos) = fmtPos pos "undefined"
    displayException (GSExcImplementationFailure pos err) = fmtPos pos err

throwGSerror (GSErrUnimpl pos) = throw $ GSExcUndefined pos
throwGSerror err = throw $ GSExcImplementationFailure $gshere $ "throwGSerror (" ++ show err ++ ") next"

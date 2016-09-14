{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module GSI.Result (GSError(..), GSResult(..), implementationFailure, stCode) where

import Language.Haskell.TH.Syntax (location)
import Language.Haskell.TH.Lib (appE, conE)

import GSI.Util (Pos, gsfmtLocation)

data GSResult a
  = GSImplementationFailure Pos String
  | GSError GSError

data GSError = GSErrUnimpl Pos

stCode :: GSResult a -> String
stCode GSImplementationFailure{} = "GSImplementationFailure"
stCode GSError{} = "GSError"

implementationFailure = do
    loc <- location
    conE 'GSImplementationFailure `appE` gsfmtLocation loc

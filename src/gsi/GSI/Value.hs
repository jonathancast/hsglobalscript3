{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module GSI.Value (GSValue(..), GSError(..), GSThunkState(..), gsundefined_w, gsapply, gsapply_w, gsundefined, gsimplementationFailure, gstoplevelclosure, gstoplevelclosure_w, gsvCode, gstsCode) where

import Control.Concurrent (MVar, newMVar)

import Language.Haskell.TH.Lib (appE, conE, varE)

import GSI.Util (Pos, gshere, gsfatal)
import GSI.RTS (Event)
import {-# SOURCE #-} GSI.ByteCode (GSBCO, ToGSBCO(..))

data GSValue
  = GSImplementationFailure Pos String
  | GSError GSError
  | GSThunk GSThunk
  | GSClosure Pos GSBCO

data GSError = GSErrUnimpl Pos
  deriving (Show)

type GSThunk = MVar GSThunkState

data GSThunkState
  = GSApply Pos GSValue [GSValue]
  | GSTSStack Event
  | GSTSIndirection GSValue

gsundefined = varE 'gsundefined_w `appE` gshere
gsundefined_w pos = GSError (GSErrUnimpl pos)

gsimplementationFailure = conE 'GSImplementationFailure `appE` gshere

gsapply = varE 'gsapply_w `appE` gshere

gsapply_w pos fn args = fmap GSThunk $ newMVar $ GSApply pos fn args

gstoplevelclosure = varE 'gstoplevelclosure_w `appE` gshere

gstoplevelclosure_w :: ToGSBCO bc => Pos -> (GSValue -> bc) -> GSValue
gstoplevelclosure_w pos f = GSClosure pos (gsbco f)

gsvCode :: GSValue -> String
gsvCode GSImplementationFailure{} = "GSImplementationFailure"
gsvCode GSError{} = "GSError"
gsvCode GSThunk{} = "GSThunk"
gsvCode GSClosure{} = "GSClosure"

gstsCode :: GSThunkState -> String
gstsCode GSApply{} = "GSApply"
gstsCode GSTSStack{} = "GSTSStack"
gstsCode GSTSIndirection{} = "GSTSIndirection"

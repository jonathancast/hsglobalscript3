{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module GSI.Value (GSValue(..), GSThunkState(..), gsapply, gsapply_w, gsundefined, gsimplementationFailure, gstoplevelclosure, gstoplevelclosure_w, gsvCode, gstsCode) where

import Control.Concurrent (MVar, newMVar)

import Language.Haskell.TH.Lib (appE, conE, varE)

import GSI.Util (Pos, gshere, gsfatal)
import GSI.RTS (Event)

data GSValue a
  = GSUndefined Pos
  | GSImplementationFailure Pos String
  | GSThunk (MVar (GSThunkState a))

data GSThunkState a
  = GSApply Pos (GSValue a) [GSValue a]
  | GSTSStack Event
  | GSTSIndirection (GSValue a)

gsundefined = conE 'GSUndefined `appE` gshere
gsimplementationFailure = conE 'GSImplementationFailure `appE` gshere

gsapply = varE 'gsapply_w `appE` gshere

gsapply_w pos fn args = fmap GSThunk $ newMVar $ GSApply pos fn args

gstoplevelclosure = varE 'gstoplevelclosure_w `appE` gshere

gstoplevelclosure_w :: (GSValue a -> bc) -> GSValue a
gstoplevelclosure_w = $gsfatal "gstoplevelclosure_w next"

gsvCode :: GSValue a -> String
gsvCode GSUndefined{} = "GSUndefined"
gsvCode GSImplementationFailure{} = "GSImplementationFailure"
gsvCode GSThunk{} = "GSThunk"

gstsCode :: GSThunkState a -> String
gstsCode GSApply{} = "GSApply"
gstsCode GSTSStack{} = "GSTSStack"
gstsCode GSTSIndirection{} = "GSTSIndirection"

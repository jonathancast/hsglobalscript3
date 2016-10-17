{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module GSI.Value (GSValue(..), gsapply, gsundefined, gsvCode) where

import Control.Concurrent (MVar, newMVar)

import Language.Haskell.TH.Lib (appE, conE, varE)

import GSI.Util (Pos, gshere, gsfatal)

data GSValue a
  = GSUndefined Pos
  | GSThunk (MVar (GSThunkState a))

data GSThunkState a
  = GSApply Pos (GSValue a) [GSValue a]

gsundefined = conE 'GSUndefined `appE` gshere

gsapply = varE 'gsapply_w `appE` gshere

gsapply_w pos fn args = fmap GSThunk $ newMVar $ GSApply pos fn args

gsvCode :: GSValue a -> String
gsvCode GSUndefined{} = "GSUndefined"
gsvCode GSThunk{} = "GSThunk"

gstsCode :: GSThunkState a -> String
gstsCode = $gsfatal "gstsCode next"

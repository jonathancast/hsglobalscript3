{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module GSI.ByteCode (GSBCO, ToGSBCO(..), gsbcundefined, gsbcundefined_w) where

import Language.Haskell.TH.Lib (appE, varE)

import GSI.Util (Pos, gsfatal, gshere)
import GSI.Value (GSValue)

data GSBCO

class ToGSBCO r where
    gsbco :: r -> GSBCO

instance ToGSBCO r => ToGSBCO (GSValue -> r) where
    gsbco = $gsfatal "gsbco next"

instance ToGSBCO GSBCO where
    gsbco = id

gsbcundefined = varE 'gsbcundefined_w `appE` gshere

gsbcundefined_w :: Pos -> GSBCO
gsbcundefined_w = $gsfatal "gsbcundefined_w next"

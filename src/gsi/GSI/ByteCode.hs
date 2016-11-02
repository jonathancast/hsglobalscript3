{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module GSI.ByteCode (GSBCO, ToGSBCO(..), gsbcundefined, gsbcundefined_w) where

import Language.Haskell.TH.Lib (appE, varE)

import GSI.Util (Pos, gsfatal, gshere)
import GSI.Value (GSValue)

data GSBCO a

class ToGSBCO r a where
    gsbco :: r -> GSBCO a

instance ToGSBCO r a => ToGSBCO (GSValue a -> r) a where
    gsbco = $gsfatal "gsbco next"

instance ToGSBCO (GSBCO a) a where
    gsbco = id

gsbcundefined = varE 'gsbcundefined_w `appE` gshere

gsbcundefined_w :: Pos -> GSBCO a
gsbcundefined_w = $gsfatal "gsbcundefined_w next"

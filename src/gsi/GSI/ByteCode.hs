{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module GSI.ByteCode (GSBCO, ToGSBCO(..)) where

import GSI.Util (gsfatal)
import GSI.Value (GSValue)

data GSBCO a

class ToGSBCO r a where
    gsbco :: r -> GSBCO a

instance ToGSBCO r a => ToGSBCO (GSValue a -> r) a where
    gsbco = $gsfatal "gsbco next"

instance ToGSBCO (GSBCO a) a where
    gsbco = id

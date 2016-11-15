{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, RoleAnnotations #-}
module GSI.ByteCode (GSBCO, ToGSBCO(..), bcoCode) where

import {-# SOURCE #-} GSI.Value (GSValue)

data GSBCO

class ToGSBCO r where
    gsbco :: r -> GSBCO

instance ToGSBCO r => ToGSBCO (GSValue -> r)

bcoCode :: GSBCO -> String

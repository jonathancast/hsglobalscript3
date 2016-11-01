{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, RoleAnnotations #-}
module GSI.ByteCode (GSBCO, ToGSBCO(..)) where

import {-# SOURCE #-} GSI.Value (GSValue)

type role GSBCO phantom
data GSBCO a

class ToGSBCO r a where
    gsbco :: r -> GSBCO a

instance ToGSBCO r a => ToGSBCO (GSValue a -> r) a

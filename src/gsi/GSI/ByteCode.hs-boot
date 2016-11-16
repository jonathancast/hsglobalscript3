{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, RoleAnnotations #-}
module GSI.ByteCode (GSBCO(..), ToGSBCO(..), bcoCode) where

import {-# SOURCE #-} GSI.Value (GSValue)
import GSI.ThreadType (Thread)

data GSBCO
  = GSBCOFun (GSValue -> GSBCO)
  | GSBCOExpr (IO GSValue)
  | GSBCOImp (Thread -> IO GSValue)

class ToGSBCO r where
    gsbco :: r -> GSBCO

instance ToGSBCO r => ToGSBCO (GSValue -> r)

bcoCode :: GSBCO -> String

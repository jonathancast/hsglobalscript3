{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module GSI.ByteCode (GSBCO(..), ToGSBCO(..), gsbcundefined, gsbcundefined_w, bcoCode) where

import Language.Haskell.TH.Lib (appE, varE)

import GSI.Util (Pos, gsfatal, gshere)
import GSI.Value (GSValue, gsundefined_w)

data GSBCO
  = GSBCOFun (GSValue -> GSBCO)
  | GSBCOExpr (IO GSValue)

class ToGSBCO r where
    gsbco :: r -> GSBCO

instance ToGSBCO r => ToGSBCO (GSValue -> r) where
    gsbco f = GSBCOFun (\ v -> gsbco (f v))

instance ToGSBCO GSBCO where
    gsbco = id

gsbcundefined = varE 'gsbcundefined_w `appE` gshere

gsbcundefined_w :: Pos -> GSBCO
gsbcundefined_w pos = GSBCOExpr $ return $ gsundefined_w pos

bcoCode :: GSBCO -> String
bcoCode GSBCOFun{} = "GSBCOFun"
bcoCode GSBCOExpr{} = "GSBCOExpr"

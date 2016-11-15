{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module GSI.ByteCode (GSBCO(..), ToGSBCO(..), gsbcundefined, gsbcundefined_w, gsbcbody, gsbcbody_w, bcoCode) where

import Language.Haskell.TH.Lib (appE, varE)

import GSI.Util (Pos, gsfatal, gshere)
import GSI.Value (GSValue(..), gsundefined_w)

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

newtype GSBCImp a = GSBCImp (IO a)
    deriving (Monad, Applicative, Functor)

gsbcbody = varE 'gsbcbody_w `appE` gshere

gsbcbody_w :: Pos -> GSBCO -> GSBCImp GSValue
gsbcbody_w pos bco = return $ GSImplementationFailure pos $ "gsbcbody_w " ++ bcoCode bco ++ " next"

bcoCode :: GSBCO -> String
bcoCode GSBCOFun{} = "GSBCOFun"
bcoCode GSBCOExpr{} = "GSBCOExpr"

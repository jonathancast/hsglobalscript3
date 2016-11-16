{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module GSI.ByteCode (GSBCO(..), ToGSBCO(..), gsbcundefined, gsbcundefined_w, gsbcbody, gsbcbody_w, bcoCode) where

import Language.Haskell.TH.Lib (appE, varE)

import GSI.Util (Pos, gsfatal, gshere)
import GSI.Value (GSValue(..), gsundefined_w)
import GSI.ThreadType (Thread)

data GSBCO
  = GSBCOFun (GSValue -> GSBCO)
  | GSBCOExpr (IO GSValue) -- NB: return value is §emph{equal to} enclosing expression; computes and returns its own value
  | GSBCOImp (Thread -> IO GSValue) -- NB: return value §emph{result of} enclosing expression; computes and returns a different value

class ToGSBCO r where
    gsbco :: r -> GSBCO

instance ToGSBCO r => ToGSBCO (GSValue -> r) where
    gsbco f = GSBCOFun (\ v -> gsbco (f v))

instance ToGSBCO GSBCO where
    gsbco = id

gsbcundefined = varE 'gsbcundefined_w `appE` gshere

gsbcundefined_w :: Pos -> GSBCO
gsbcundefined_w pos = GSBCOExpr $ return $ gsundefined_w pos

newtype GSBCImp a = GSBCImp { runGSBCImp :: Thread -> IO a }

instance Functor GSBCImp where
    fmap f ax = GSBCImp $ \ t -> fmap f $ runGSBCImp ax t

instance Applicative GSBCImp where
    pure x = GSBCImp (const $ pure x)
    af <*> ax = GSBCImp $ \ t -> runGSBCImp af t <*> runGSBCImp ax t

instance Monad GSBCImp where
    return x = GSBCImp (const $ return x)
    a >>= f = GSBCImp $ \ t -> runGSBCImp a t >>= \ x -> runGSBCImp (f x) t 

instance ToGSBCO (GSBCImp GSValue) where
    gsbco (GSBCImp a) = GSBCOImp a

gsbcbody = varE 'gsbcbody_w `appE` gshere

gsbcbody_w :: Pos -> GSBCO -> GSBCImp GSValue
gsbcbody_w pos bco = return $ GSImplementationFailure pos $ "gsbcbody_w " ++ bcoCode bco ++ " next"

bcoCode :: GSBCO -> String
bcoCode GSBCOFun{} = "GSBCOFun"
bcoCode GSBCOExpr{} = "GSBCOExpr"
bcoCode GSBCOImp{} = "GSBCOImp"

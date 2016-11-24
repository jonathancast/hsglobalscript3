{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module GSI.ByteCode (
    gsbcundefined, gsbcundefined_w, gsbcapply, gsbcapply_w, gsbcvar, gsbcvar_w,
    gsbcbody, gsbcbody_w
  ) where

import Language.Haskell.TH.Lib (appE, varE)

import GSI.Util (Pos, gsfatal, gshere)
import GSI.Value (GSValue(..), GSBCO(..), ToGSBCO(..), gsundefined_w, gsclosure_w)
import GSI.ThreadType (Thread)
import ACE (aceApply)
import API (apiCallBCO)

gsbcundefined = varE 'gsbcundefined_w `appE` gshere

gsbcundefined_w :: Pos -> GSBCO
gsbcundefined_w pos = GSBCOExpr $ return $ gsundefined_w pos

gsbcapply = varE 'gsbcapply_w `appE` gshere

gsbcapply_w :: ToGSBCO bco => Pos -> GSValue -> [bco] -> GSBCO
gsbcapply_w pos f args = GSBCOExpr $ mapM (gsclosure_w pos) args >>= aceApply pos f

gsbcvar = varE 'gsbcvar_w `appE` gshere

gsbcvar_w :: Pos -> GSValue -> GSBCO
gsbcvar_w pos v = GSBCOExpr $ return v

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
gsbcbody_w pos bco = GSBCImp $ apiCallBCO pos bco

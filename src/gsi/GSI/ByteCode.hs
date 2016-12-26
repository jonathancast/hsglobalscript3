{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module GSI.ByteCode (
    gsbcundefined, gsbcundefined_w, gsbclambda, gsbclambda_w, gsbcapply, gsbcapply_w, gsbcprim, gsbcprim_w, gsbcimpprim, gsbcimpprim_w, gsbcvar, gsbcvar_w, gsbcforce, gsbcforce_w,
    gsbcimplet, gsbcimplet_w, gsbcimpbind, gsbcimpbind_w, gsbcimpbody, gsbcimpbody_w,
    gsbcviewpattern, gsbcviewpattern_w
  ) where

import Language.Haskell.TH.Lib (appE, varE)

import GSI.Util (Pos, gsfatal, gshere)
import GSI.Value (GSValue(..), GSBCO(..), GSStackFrame(..), ToGSBCO(..), gsundefined_w, gsclosure_w)
import GSI.ThreadType (Thread)
import ACE (aceEnter, aceEnterBCO, aceThrow)
import API (apiCallBCO)

gsbcundefined = varE 'gsbcundefined_w `appE` gshere

gsbcundefined_w :: Pos -> GSBCO
gsbcundefined_w pos = GSBCOExpr $ aceThrow $ gsundefined_w pos

gsbclambda = varE 'gsbclambda_w `appE` gshere

gsbclambda_w :: ToGSBCO bco => Pos -> (GSValue -> bco) -> GSBCO
gsbclambda_w pos fn = GSBCOExpr $ \ st -> do
    v <- gsclosure_w pos fn
    aceEnter pos v st

gsbcapply = varE 'gsbcapply_w `appE` gshere

gsbcapply_w :: ToGSBCO bco => Pos -> GSValue -> [bco] -> GSBCO
gsbcapply_w pos f args = GSBCOExpr $ \ st -> do
    asv <- mapM (gsclosure_w pos) args
    aceEnter pos f (map (GSStackArg pos) asv ++ st)

gsbcapp_w :: (ToGSBCO bco0, ToGSBCO bco1) => Pos -> bco0 -> [bco1] -> GSBCO
gsbcapp_w pos f args = GSBCOExpr $ \ st-> do
    asv <- mapM (gsclosure_w pos) args
    aceEnterBCO pos (gsbco f) (map (GSStackArg pos) asv ++ st)

gsbcprim = varE 'gsbcprim_w `appE` gshere

class GSBCPrimType f r where
    gsbcprim_ww :: Pos -> f -> r

instance GSBCPrimType (IO GSValue) GSBCO where
    gsbcprim_ww pos f = GSBCOExpr $ \ st -> do
        v <- f
        aceEnter pos v st

instance GSBCPrimType f r => GSBCPrimType (GSValue -> f) (GSValue -> r) where
    gsbcprim_ww pos f v = gsbcprim_ww pos (f v)

gsbcprim_w :: GSBCPrimType f r => Pos -> (Pos -> f) -> r
gsbcprim_w pos f = gsbcprim_ww pos (f pos)

gsbcimpprim = varE 'gsbcimpprim_w `appE` gshere

class GSBCImpPrimType f r where
    gsbcimpprim_ww :: (Thread -> f) -> r

instance GSBCImpPrimType (IO GSValue) GSBCO where
    gsbcimpprim_ww f = GSBCOImp f

gsbcimpprim_w :: GSBCImpPrimType f r => Pos -> (Pos -> Thread -> f) -> r
gsbcimpprim_w pos f = gsbcimpprim_ww (f pos)

gsbcvar = varE 'gsbcvar_w `appE` gshere

gsbcvar_w :: Pos -> GSValue -> GSBCO
gsbcvar_w pos v = GSBCOExpr $ \ st -> aceEnter pos v st

gsbcforce = varE 'gsbcforce_w `appE` gshere

gsbcforce_w :: (ToGSBCO a, ToGSBCO b) => Pos -> a -> (GSValue -> b) -> GSBCO
gsbcforce_w pos e k = GSBCOExpr $ \ st -> aceEnterBCO pos (gsbco e) (GSStackForce pos (gsbco . k) : st)

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

gsbcimplet = varE 'gsbcimplet_w `appE` gshere

gsbcimplet_w :: ToGSBCO bco => Pos -> bco -> GSBCImp GSValue
gsbcimplet_w pos bco = GSBCImp $ \ _ -> gsclosure_w pos bco

gsbcimpbind = varE 'gsbcimpbind_w `appE` gshere

gsbcimpbind_w :: ToGSBCO bco => Pos -> bco -> GSBCImp GSValue
gsbcimpbind_w pos bco = GSBCImp $ apiCallBCO pos $ gsbco bco

gsbcimpbody = varE 'gsbcimpbody_w `appE` gshere

gsbcimpbody_w :: ToGSBCO bco => Pos -> bco -> GSBCImp GSValue
gsbcimpbody_w pos bco = GSBCImp $ apiCallBCO pos $ gsbco bco

gsbcviewpattern = varE 'gsbcviewpattern_w `appE` gshere

gsbcviewpattern_w :: (ToGSBCO bco, ToGSViewPattern res) => Pos -> bco -> res
gsbcviewpattern_w pos v =
    gsbcviewpattern_ww pos (\ sk -> gsbcapp_w pos v [ gsbcundefined_w $gshere, gsbcundefined_w $gshere ]) -- §hs{fail}, §hs{sk success}

class ToGSViewPattern res where
    gsbcviewpattern_ww :: ToGSBCO bco => Pos -> (GSBCO -> bco) -> res

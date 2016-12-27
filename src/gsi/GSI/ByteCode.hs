{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module GSI.ByteCode (
    gsbcundefined, gsbcundefined_w, gsbclambda, gsbclambda_w, gsbcapply, gsbcapply_w, gsbcprim, gsbcprim_w, gsbcimpprim, gsbcimpprim_w, gsbcvar, gsbcvar_w, gsbcforce, gsbcforce_w, gsbcimplementationfailure, gsbcimplementationfailure_w,
    gsbcimplet, gsbcimplet_w, gsbcimpbind, gsbcimpbind_w, gsbcimpbody, gsbcimpbody_w,
    gsbcconstr_view, gsbcconstr_view_w, gsbcconstr_view_ww,
    gsbcviewpattern, gsbcviewpattern_w
  ) where

import Language.Haskell.TH.Lib (appE, varE)

import GSI.Util (Pos, gsfatal, gshere)
import GSI.Syn (GSVar, gsvar)
import GSI.Value (GSValue(..), GSBCO(..), GSStackFrame(..), ToGSBCO(..), gsundefined_w, gsclosure_w, gsvCode)
import GSI.ThreadType (Thread)
import ACE (aceEnter, aceEnterBCO, aceThrow)
import API (apiCallBCO)

gsbcundefined = varE 'gsbcundefined_w `appE` gshere

gsbcundefined_w :: Pos -> GSBCO
gsbcundefined_w pos = GSBCOExpr $ aceThrow $ gsundefined_w pos

gsbcimplementationfailure = varE 'gsbcimplementationfailure_w `appE` gshere

gsbcimplementationfailure_w :: Pos -> String -> GSBCO
gsbcimplementationfailure_w pos msg = GSBCOExpr $ aceThrow $ GSImplementationFailure pos msg

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

gsbcconstr_view = varE 'gsbcconstr_view_w `appE` gshere

gsbcconstr_view_w pos = gsbcconstr_view_ww pos . gsvar

gsbcconstr_view_ww :: Pos -> GSVar -> GSValue -> GSValue -> GSValue -> GSBCO
gsbcconstr_view_ww pos c ek sk x = gsbcforce_w pos (gsbcvar_w pos x) $ \ x0 -> case x0 of
        _ -> gsbcimplementationfailure_w $gshere $ "gsbcconstr_view_ww " ++ gsvCode x0 ++ " next"

gsbcviewpattern = varE 'gsbcviewpattern_w `appE` gshere

gsbcviewpattern_w :: (ToGSBCO bco, ToGSViewPattern res) => Pos -> bco -> res
gsbcviewpattern_w pos v =
    gsbcviewpattern_ww pos (\ sk -> gsbcapp_w pos v [ gsbcimplementationfailure_w $gshere "fail next", gsbcimplementationfailure_w $gshere "sk success next" ]) -- §hs{fail}, §hs{sk success}

class ToGSViewPattern res where
    gsbcviewpattern_ww :: Pos -> (GSBCO -> GSBCO) -> res

instance (ToGSBCO bco, ToGSViewPattern res) => ToGSViewPattern (bco -> res) where
    gsbcviewpattern_ww pos k p = gsbcviewpattern_ww pos (\ sk -> k (gsbcimplementationfailure_w $gshere "recursion case next")) -- §gs{λ 'η 'x. sk (η ∧ p x)}

instance ToGSViewPattern GSBCO where
    gsbcviewpattern_ww pos k = k (gsbcimplementationfailure_w $gshere "base case next") -- §gs{λ 'η. η}

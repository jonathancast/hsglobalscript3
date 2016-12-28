{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module GSI.ByteCode (
    gsbcundefined, gsbcundefined_w, gsbclambda, gsbclambda_w, gsbcapply, gsbcapply_w, gsbcprim, gsbcprim_w, gsbcimpprim, gsbcimpprim_w, gsbcvar, gsbcforce, gsbcforce_w, gsbcimplementationfailure, gsbcimplementationfailure_w,
    gsbcoimpfor, gsbcimplet, gsbcimplet_w, gsbcimpbind, gsbcimpbind_w, gsbcimpbody, gsbcimpbody_w,
    gsbcconstr_view, gsbcconstr_view_w, gsbcconstr_view_ww,
    gsbcviewpattern, gsbcviewpattern_w, gsbcvarpattern, gsbcvarpattern_w
  ) where

import Language.Haskell.TH.Lib (appE, varE, conE)

import GSI.Util (Pos, gsfatal, gshere)
import GSI.Syn (GSVar, gsvar, fmtVarAtom)
import GSI.Value (GSValue(..), GSBCO(..), GSStackFrame(..), ToGSBCO(..), gsimplementationfailure, gsundefined_w, gsclosure_w, gsvCode, bcoCode)
import GSI.ThreadType (Thread)
import GSI.CalculusPrims (gsparand)
import ACE (aceEnter, aceEnterBCO, aceThrow)
import API (apiCallBCO)

gsbcundefined = varE 'gsbcundefined_w `appE` gshere

gsbcundefined_w :: Pos -> GSBCO
gsbcundefined_w pos = GSBCOExpr $ aceThrow $ gsundefined_w pos

gsbcimplementationfailure = varE 'gsbcimplementationfailure_w `appE` gshere

gsbcimplementationfailure_w :: Pos -> String -> GSBCO
gsbcimplementationfailure_w pos msg = GSBCOExpr $ aceThrow $ GSImplementationFailure pos msg

gsbclambda = varE 'gsbclambda_w `appE` gshere

gsbclambda_w :: Pos -> (GSValue -> GSBCO) -> GSBCO
gsbclambda_w pos fn = GSBCOExpr $ \ st -> do
    v <- gsclosure_w pos fn
    aceEnter pos v st

gsbcapply = varE 'gsbcapply_w `appE` gshere

gsbcapply_w :: Pos -> GSValue -> [GSBCO] -> GSBCO
gsbcapply_w pos f args = GSBCOExpr $ \ st -> do
    asv <- mapM (gsclosure_w pos) args
    aceEnter pos f (map (GSStackArg pos) asv ++ st)

gsbcapp_w :: Pos -> GSBCO -> [GSBCO] -> GSBCO
gsbcapp_w pos f args = GSBCOExpr $ \ st-> do
    asv <- mapM (gsclosure_w pos) args
    aceEnterBCO pos f (map (GSStackArg pos) asv ++ st)

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

gsbcvar = conE 'GSBCOVar `appE` gshere

gsbcforce = varE 'gsbcforce_w `appE` gshere

gsbcforce_w :: Pos -> GSBCO -> (GSValue -> GSBCO) -> GSBCO
gsbcforce_w pos e k = GSBCOExpr $ \ st -> aceEnterBCO pos e (GSStackForce pos k : st)

gsbclet_w :: Pos -> GSBCO -> (GSValue -> GSBCO) -> GSBCO
gsbclet_w pos e k = GSBCOExpr $ \ st -> do
    v <- gsclosure_w pos e
    case k v of
        GSBCOExpr e' -> e' st
        bco -> return $ $gsimplementationfailure $ "gsbclet_w " ++ bcoCode bco ++ " next"

newtype GSBCImp a = GSBCImp { runGSBCImp :: Thread -> IO a }

instance Functor GSBCImp where
    fmap f ax = GSBCImp $ \ t -> fmap f $ runGSBCImp ax t

instance Applicative GSBCImp where
    pure x = GSBCImp (const $ pure x)
    af <*> ax = GSBCImp $ \ t -> runGSBCImp af t <*> runGSBCImp ax t

instance Monad GSBCImp where
    return x = GSBCImp (const $ return x)
    a >>= f = GSBCImp $ \ t -> runGSBCImp a t >>= \ x -> runGSBCImp (f x) t

gsbcoimpfor :: GSBCImp GSValue -> GSBCO
gsbcoimpfor (GSBCImp a) = GSBCOImp a

gsbcimplet = varE 'gsbcimplet_w `appE` gshere

gsbcimplet_w :: Pos -> GSBCO -> GSBCImp GSValue
gsbcimplet_w pos bco = GSBCImp $ \ _ -> gsclosure_w pos bco

gsbcimpbind = varE 'gsbcimpbind_w `appE` gshere

gsbcimpbind_w :: Pos -> GSBCO -> GSBCImp GSValue
gsbcimpbind_w pos bco = GSBCImp $ apiCallBCO pos $ gsbco bco

gsbcimpbody = varE 'gsbcimpbody_w `appE` gshere

gsbcimpbody_w :: Pos -> GSBCO -> GSBCImp GSValue
gsbcimpbody_w pos bco = GSBCImp $ apiCallBCO pos bco

gsbcconstr_view = varE 'gsbcconstr_view_w `appE` gshere

gsbcconstr_view_w pos = gsbcconstr_view_ww pos . gsvar

gsbcconstr_view_ww :: Pos -> GSVar -> GSValue -> GSValue -> GSValue -> GSBCO
gsbcconstr_view_ww pos c ek sk x = gsbcforce_w pos (GSBCOVar pos x) $ \ x0 -> case x0 of
        GSConstr pos1 c' as
            | c == c' -> gsbcapp_w pos (GSBCOVar pos sk) (map (GSBCOVar pos) as)
            | otherwise -> gsbcimplementationfailure_w $gshere $ ("gsbcconstr_view_ww "++) . fmtVarAtom c . (' ':) . fmtVarAtom c' . (" next"++) $ ""
        _ -> gsbcimplementationfailure_w $gshere $ "gsbcconstr_view_ww " ++ gsvCode x0 ++ " next"

gsbcviewpattern = varE 'gsbcviewpattern_w `appE` gshere

gsbcviewpattern_w :: (ToGSViewPattern res) => Pos -> GSBCO -> res
gsbcviewpattern_w pos v =
    gsbcviewpattern_ww pos (\ sk -> gsbcapp_w pos v [ gsbcimplementationfailure_w $gshere "fail next", gsbcapp_w $gshere sk [GSBCOVar pos $ GSConstr pos (gsvar "1") [$gsimplementationfailure "success next"]] ])

class ToGSViewPattern res where
    gsbcviewpattern_ww :: Pos -> (GSBCO -> GSBCO) -> res

instance (ToGSViewPattern res) => ToGSViewPattern (GSBCO -> res) where
    gsbcviewpattern_ww pos k p = gsbcviewpattern_ww pos $ \ (sk :: GSBCO) -> k $ gsbco $ \ (eta :: GSValue) (x :: GSValue) ->
        gsbclet_w pos (gsbcapp_w pos p [ GSBCOVar pos x ]) $ \ px ->
            gsbcapp_w pos sk [ gsbcprim_w pos gsparand eta px :: GSBCO ]

instance ToGSViewPattern GSBCO where
    gsbcviewpattern_ww pos k = k (gsbco $ \ eta -> GSBCOVar pos eta)

gsbcvarpattern = varE 'gsbcvarpattern_w `appE` gshere

gsbcvarpattern_w pos x = gsbcvarpattern_ww pos (gsvar x)

gsbcvarpattern_ww :: Pos -> GSVar -> GSBCO
gsbcvarpattern_ww pos v = gsbco $ \ (x::GSValue) -> GSBCOVar pos $ GSConstr pos (gsvar "1")
    [$gsimplementationfailure "singleton record next"] -- > GSRecord $ Map.fromList [ (v, x) ]

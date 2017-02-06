{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module GSI.ByteCode (
    gsbcundefined, gsbcundefined_w, gsbcarg, gsbcarg_w, gsbcapply, gsbcapply_w, gsbcprim, gsbcprim_w, gsbcimpprim, gsbcimpprim_w, gsbcforce, gsbcforce_w, gsbchere, gsbchere_w, gsbcimplementationfailure, gsbcimplementationfailure_w,
    gsbcimpfor, gsbcimplet, gsbcimplet_w, gsbcimpbind, gsbcimpbind_w, gsbcimpbody, gsbcimpbody_w,
    gsbcconstr_view, gsbcconstr_view_w, gsbcconstr_view_ww,
    gsbcviewpattern, gsbcviewpattern_w, gsbcvarpattern, gsbcvarpattern_w
  ) where

import Language.Haskell.TH.Lib (appE, varE, conE)

import GSI.Util (Pos, StackTrace(..), gsfatal, gshere)
import GSI.Syn (GSVar, gsvar, fmtVarAtom)
import GSI.Error (GSError(..))
import GSI.Value (GSValue(..), GSBCO(..), GSExpr(..), GSArg(..), GSStackFrame(..), GSBCImp(..), gsimplementationfailure, gsundefined_w, gslambda_w, gsprepare_w, gsthunk_w, gsimpfor_w, gsvCode, argCode)
import GSI.ThreadType (Thread)
import GSI.CalculusPrims (gsparand)
import ACE (aceEnter, aceEnterExpr, aceReturn, aceThrow)
import API (apiCall, apiCallExpr, apiImplementationFailure)

gsbcundefined = varE 'gsbcundefined_w `appE` gshere

gsbcundefined_w :: Pos -> GSExpr
gsbcundefined_w pos = GSExpr $ \ st cs -> aceThrow (GSError (GSErrUnimpl (StackTrace pos cs))) st

gsbchere = varE 'gsbchere_w `appE` gshere

gsbchere_w :: Pos -> GSExpr
gsbchere_w pos = GSExpr $ \ st cs -> aceThrow ($gsimplementationfailure "gsbchere next") st

gsbcimplementationfailure = varE 'gsbcimplementationfailure_w `appE` gshere

gsbcimplementationfailure_w :: Pos -> String -> GSExpr
gsbcimplementationfailure_w pos msg = GSExpr $ \ st cs -> aceThrow (GSImplementationFailure pos msg) st

gsbcarg = varE 'gsbcarg_w `appE` gshere

gsbcarg_w :: Pos -> (GSValue -> GSExpr) -> GSExpr
gsbcarg_w pos fn = GSExpr $ \ st cs -> do
    aceEnter [ StackTrace pos cs ] (gslambda_w pos fn) st

gsbcapply = varE 'gsbcapply_w `appE` gshere

gsbcapply_w :: Pos -> GSValue -> [GSArg] -> GSExpr
gsbcapply_w pos f args = GSExpr $ \ st cs -> do
    asv <- mapM (gsprepare_w pos) args
    aceEnter [ StackTrace pos cs ] f (map (GSStackArg (StackTrace pos cs)) asv ++ st)

gsbcapp_w :: Pos -> GSExpr -> [GSArg] -> GSExpr
gsbcapp_w pos f args = GSExpr $ \ st cs -> do
    asv <- mapM (gsprepare_w pos) args
    aceEnterExpr [StackTrace pos cs] f (map (GSStackArg (StackTrace pos cs)) asv ++ st)

gsbcprim = varE 'gsbcprim_w `appE` gshere

class GSBCPrimType f r where
    gsbcprim_ww :: Pos -> f -> r

instance GSBCPrimType (IO GSValue) GSExpr where
    gsbcprim_ww pos f = GSExpr $ \ st cs -> do
        v <- f
        aceEnter [ StackTrace pos cs ] v st

instance GSBCPrimType f r => GSBCPrimType (GSValue -> f) (GSValue -> r) where
    gsbcprim_ww pos f v = gsbcprim_ww pos (f v)

gsbcprim_w :: GSBCPrimType f r => Pos -> (Pos -> f) -> r
gsbcprim_w pos f = gsbcprim_ww pos (f pos)

gsbcimpprim = varE 'gsbcimpprim_w `appE` gshere

class GSBCImpPrimType f r where
    gsbcimpprim_ww :: Pos -> (Thread -> f) -> r

instance GSBCImpPrimType (IO GSValue) GSExpr where
    gsbcimpprim_ww pos f = GSExpr $ \ st cs -> aceReturn (GSClosure [StackTrace pos []] (GSImp f)) st

gsbcimpprim_w :: GSBCImpPrimType f r => Pos -> (Pos -> Thread -> f) -> r
gsbcimpprim_w pos f = gsbcimpprim_ww pos (f pos)

gsbcforce = varE 'gsbcforce_w `appE` gshere

gsbcforce_w :: Pos -> GSArg -> (GSValue -> GSExpr) -> GSExpr
gsbcforce_w pos e k = GSExpr $ \ st cs -> case e of
    GSArgExpr pos' e' -> aceEnterExpr [StackTrace pos cs] e' (GSStackForce pos k : st)
    GSArgVar v -> aceEnter [StackTrace pos cs] v (GSStackForce pos k : st)
    _ -> aceThrow ($gsimplementationfailure $ "gsbcforce_w " ++ argCode e ++ " next") st

gsbclet_w :: Pos -> GSExpr -> (GSValue -> GSExpr) -> GSExpr
gsbclet_w pos e k = GSExpr $ \ st cs -> do
    v <- gsthunk_w pos e
    aceEnterExpr [StackTrace pos cs] (k v) st

gsbcimpfor = varE 'gsbcimpfor_w `appE` gshere

gsbcimpfor_w :: Pos -> GSBCImp GSValue -> GSExpr
gsbcimpfor_w pos a = GSExpr $ \ st cs -> aceReturn (gsimpfor_w pos a) st

gsbcimplet = varE 'gsbcimplet_w `appE` gshere

gsbcimplet_w :: Pos -> GSArg -> GSBCImp GSValue
gsbcimplet_w pos a = GSBCImp $ \ _ -> gsprepare_w pos a

gsbcimpbind = varE 'gsbcimpbind_w `appE` gshere

gsbcimpbind_w :: Pos -> GSArg -> GSBCImp GSValue
gsbcimpbind_w pos (GSArgVar v) = GSBCImp $ \ t -> apiCall pos v t
gsbcimpbind_w pos a = GSBCImp $ \t -> $apiImplementationFailure $ "gsbcimpbind_w " ++ argCode a ++ " next"

gsbcimpbody = varE 'gsbcimpbody_w `appE` gshere

gsbcimpbody_w :: Pos -> GSArg -> GSBCImp GSValue
gsbcimpbody_w pos0 (GSArgExpr pos1 e) = GSBCImp $ \ t -> apiCallExpr pos0 e t
gsbcimpbody_w pos a = GSBCImp $ \t -> $apiImplementationFailure $ "gsbcimpbody_w " ++ argCode a ++ " next"

gsbcconstr_view = varE 'gsbcconstr_view_w `appE` gshere

gsbcconstr_view_w pos = gsbcconstr_view_ww pos . gsvar

gsbcconstr_view_ww :: Pos -> GSVar -> GSValue -> GSValue -> GSValue -> GSExpr
gsbcconstr_view_ww pos c ek sk x = gsbcforce_w pos (GSArgVar x) $ \ x0 -> case x0 of
        GSConstr pos1 c' as
            | c == c' -> gsbcapply_w pos sk (map GSArgVar as)
            | otherwise -> gsbcimplementationfailure_w $gshere $ ("gsbcconstr_view_ww "++) . fmtVarAtom c . (' ':) . fmtVarAtom c' . (" next"++) $ ""
        _ -> gsbcimplementationfailure_w $gshere $ "gsbcconstr_view_ww " ++ gsvCode x0 ++ " next"

gsbcviewpattern = varE 'gsbcviewpattern_w `appE` gshere

gsbcviewpattern_w :: (ToGSViewPattern res) => Pos -> GSValue -> res
gsbcviewpattern_w pos v =
    gsbcviewpattern_ww pos (\ sk -> gsbcapply_w pos v [ GSArgExpr pos (gsbcimplementationfailure_w $gshere "fail next"), GSArgExpr pos (gsbcapp_w $gshere sk [GSArgVar $ GSConstr pos (gsvar "1") [$gsimplementationfailure "success next"]]) ])

class ToGSViewPattern res where
    gsbcviewpattern_ww :: Pos -> (GSExpr -> GSExpr) -> res

instance (ToGSViewPattern res) => ToGSViewPattern (GSExpr -> res) where
    gsbcviewpattern_ww pos k p = gsbcviewpattern_ww pos $ \ (sk :: GSExpr) -> k $ gsbcarg_w pos $ \ eta  -> gsbcarg_w pos $ \ x ->
        gsbclet_w pos (gsbcapp_w pos p [ GSArgVar x ]) $ \ px ->
            gsbcapp_w pos sk [ GSArgExpr pos (gsbcprim_w pos gsparand eta px) ]

instance ToGSViewPattern GSExpr where
    gsbcviewpattern_ww pos k = k (gsbcarg_w pos $ \ eta -> GSExpr $ \ st cs -> aceEnter [StackTrace pos cs] eta st)

gsbcvarpattern = varE 'gsbcvarpattern_w `appE` gshere

gsbcvarpattern_w pos x = gsbcvarpattern_ww pos (gsvar x)

gsbcvarpattern_ww :: Pos -> GSVar -> GSExpr
gsbcvarpattern_ww pos v = gsbcarg_w pos $ \ x -> GSExpr $ \  st cs->
    let res = GSConstr pos (gsvar "1") [$gsimplementationfailure "singleton record next"] -- > GSRecord $ Map.fromList [ (v, x) ]
    in aceReturn res st

{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module GSI.ByteCode (
    gsbcundefined, gsbcundefined_w, gsbcarg, gsbcarg_w, gsbcenter, gsbcenter_w, gsbcapply, gsbcapply_w, gsbcprim, gsbcprim_w, gsbcimpprim, gsbcimpprim_w, gsbcforce, gsbcforce_w, gsbclfield, gsbclfield_w, gsbcfield, gsbcfield_w, gsbcrecord, gsbcrecord_w, gsbcconstr, gsbcconstr_w, gsbcexternal, gsbcexternal_w, gsbchere, gsbchere_w, gsbcerror, gsbcerror_w, gsbcimplementationfailure, gsbcimplementationfailure_w,
    gsbccomposegen_w, gsbcexecgen_w, gsbcvarbind_w, gsbcemptygen_w,
    gsbcevalnatural, gsbcevalnatural_w, gsbcfmterrormsg, gsbcfmterrormsg_w,
    gsbcimpfor, gsbcimplet, gsbcimplet_w, gsbcimpbind, gsbcimpbind_w, gsbcimpbody, gsbcimpbody_w,
    gsbccomposeimpgen_w, gsbcimpexecbind_w, gsbcimpvarbind_w, gsbcemptyimpgen_w,
    gsbcconstr_view, gsbcconstr_view_w, gsbcconstr_view_ww,
    gsbcviewpattern, gsbcviewpattern_w, gsbcvarpattern, gsbcvarpattern_w
  ) where

import Control.Monad (forM)

import qualified Data.Map as Map

import Language.Haskell.TH.Lib (appE, varE)

import GSI.Util (Pos, StackTrace(..), gsfatal, gshere, fmtPos, filename, line, col)
import GSI.Syn (GSVar, gsvar, fmtVarAtom)
import GSI.Error (GSError(..))
import GSI.Value (GSValue(..), GSBCO(..), GSExpr(..), GSExternal(..), GSArg(..), GSStackFrame(..), GSBCImp(..), gsimplementationfailure, gsundefined_value_w, gslambda, gslambda_w, gsprepare_w, gsthunk_w, gsfield_w, gsimpfor_w, gsae, gsav, gsvCode, argCode)
import GSI.Functions (gsstring, gsnatural, gsfmterrormsg)
import GSI.ThreadType (Thread)
import GSI.CalculusPrims (gsparand, gsmergeenv)
import ACE (aceEnter, aceEnterExpr, aceReturn, aceThrow)
import API (apiCall, apiCallExpr, apiImplementationFailure)

gsbcundefined = varE 'gsbcundefined_w `appE` gshere

gsbcundefined_w :: Pos -> GSExpr
gsbcundefined_w pos = GSExpr $ \ st cs -> aceThrow (GSError (GSErrUnimpl (StackTrace pos cs))) st

gsbchere = varE 'gsbchere_w `appE` gshere

gsbchere_w :: Pos -> GSExpr
gsbchere_w pos = GSExpr $ \ st cs -> aceReturn res st where
    res = GSRecord $gshere $ Map.fromList [
        (gsvar "filename", $gsstring (filename pos)),
        (gsvar "line", $gsnatural (line pos)),
        (gsvar "col", $gsnatural (col pos))
      ]

gsbcrecord = varE 'gsbcrecord_w `appE` gshere

gsbcrecord_w :: Pos -> [(GSVar, GSArg)] -> GSExpr
gsbcrecord_w pos fs = GSExpr $ \ st cs -> do
    fs' <- forM fs $ \ (v, fa) -> do
        fv <- gsprepare_w pos fa
        return (v, fv)
    return $ GSRecord pos $ Map.fromList fs'

gsbcconstr = varE 'gsbcconstr_w `appE` gshere

gsbcconstr_w :: Pos -> GSVar -> [GSArg] -> GSExpr
gsbcconstr_w pos c as = GSExpr $ \ st cs -> do
    asv <- mapM (gsprepare_w pos) as
    aceReturn (GSConstr pos c asv) st

gsbcexternal = varE 'gsbcexternal_w `appE` gshere

gsbcexternal_w :: GSExternal e => Pos -> e -> GSExpr
gsbcexternal_w pos e = GSExpr $ \ st cs -> aceReturn (GSExternal (toExternal e)) st

gsbcerror = varE 'gsbcerror_w `appE` gshere

gsbcerror_w :: Pos -> Pos -> String -> GSExpr
gsbcerror_w _ pos msg = GSExpr $ \ st cs -> aceThrow (GSError (GSErrError pos msg)) st

gsbcimplementationfailure = varE 'gsbcimplementationfailure_w `appE` gshere

gsbcimplementationfailure_w :: Pos -> String -> GSExpr
gsbcimplementationfailure_w pos msg = GSExpr $ \ st cs -> aceThrow (GSImplementationFailure pos msg) st

gsbcarg = varE 'gsbcarg_w `appE` gshere

gsbcarg_w :: Pos -> (GSValue -> GSExpr) -> GSExpr
gsbcarg_w pos fn = GSExpr $ \ st cs -> do
    aceEnter [ StackTrace pos cs ] (gslambda_w pos fn) st

gsbcenter = varE 'gsbcenter_w `appE` gshere

gsbcenter_w :: Pos -> GSValue -> GSExpr
gsbcenter_w pos v = GSExpr $ \ st cs -> aceEnter [ StackTrace pos cs ] v st

gsbcapply = varE 'gsbcapply_w `appE` gshere

gsbcapply_w :: Pos -> GSValue -> [GSArg] -> GSExpr
gsbcapply_w pos f args = GSExpr $ \ st cs -> do
    asv <- mapM (gsprepare_w pos) args
    aceEnter [ StackTrace pos cs ] f (map (GSStackArg (StackTrace pos cs)) asv ++ st)

gsbcapp_w :: Pos -> GSExpr -> [GSArg] -> GSExpr
gsbcapp_w pos f args = GSExpr $ \ st cs -> do
    asv <- mapM (gsprepare_w pos) args
    aceEnterExpr [StackTrace pos cs] f (map (GSStackArg (StackTrace pos cs)) asv ++ st)

gsbcapparg_w :: Pos -> GSArg -> [GSArg] -> GSExpr
gsbcapparg_w pos (GSArgVar f) as = gsbcapply_w pos f as
gsbcapparg_w pos f as = gsbcimplementationfailure_w $gshere $ "gsbcapparg_w pos " ++ argCode f ++ " as next"

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
gsbcforce_w pos e k = GSExpr $ \ st cs -> let c1 = StackTrace pos cs in case e of
    GSArgExpr pos' e' -> aceEnterExpr [c1] e' (GSStackForce c1 k : st)
    GSArgVar v -> aceEnter [c1] v (GSStackForce c1 k : st)
    _ -> aceThrow ($gsimplementationfailure $ "gsbcforce_w " ++ argCode e ++ " next") st

gsbclet_w :: Pos -> GSExpr -> (GSValue -> GSExpr) -> GSExpr
gsbclet_w pos e k = GSExpr $ \ st cs -> do
    v <- gsthunk_w pos e
    aceEnterExpr [StackTrace pos cs] (k v) st

gsbcimpfor = varE 'gsbcimpfor_w `appE` gshere

gsbcimpfor_w :: Pos -> GSBCImp GSValue -> GSExpr
gsbcimpfor_w pos a = GSExpr $ \ st cs -> aceReturn (gsimpfor_w pos a) st

gsbclfield = varE 'gsbclfield_w `appE` gshere

gsbclfield_w :: Pos -> GSVar -> GSValue -> (GSValue -> GSExpr) -> GSExpr
gsbclfield_w pos f r k = GSExpr $ \ st cs -> do
    v <- gsfield_w pos f r
    aceEnterExpr [StackTrace pos cs] (k v) st

gsbcfield = varE 'gsbcfield_w `appE` gshere

gsbcfield_w :: Pos -> GSArg -> GSVar -> GSExpr
gsbcfield_w pos a f = gsbcforce_w pos a $ \ r -> case r of
    GSRecord pos1 fs -> case Map.lookup f fs of
        Just v -> gsbcenter_w pos v
        Nothing -> gsbcimplementationfailure_w $gshere $ (fmtPos pos1 . ("missing field "++) . fmtVarAtom f) $ ""
    _ -> gsbcimplementationfailure_w $gshere $ "gsbcfield " ++ gsvCode r ++ " next"

gsbcevalnatural = varE 'gsbcevalnatural_w `appE` gshere

gsbcevalnatural_w :: Pos -> GSArg -> (Integer -> GSExpr) -> GSExpr
gsbcevalnatural_w pos na k = gsbcforce_w pos na $ \ nv -> case nv of
    GSNatural n -> k n
    _ -> gsbcimplementationfailure_w $gshere $ "gsbcevalnatural_w " ++ gsvCode nv ++ " next"

gsbcfmterrormsg = varE 'gsbcfmterrormsg_w `appE` gshere

gsbcfmterrormsg_w :: Pos -> GSArg -> (String -> GSExpr) -> GSExpr
gsbcfmterrormsg_w pos msg k = GSExpr $ \ st cs -> do
    msgv <- gsprepare_w pos msg
    msgs <- $gsfmterrormsg msgv
    aceEnterExpr [StackTrace pos cs] (k msgs) st

gsbccomposegen_w :: Pos -> GSArg -> GSArg -> (GSValue -> GSExpr) -> GSExpr
gsbccomposegen_w pos gsbind gen0 gen1 = gsbcapparg_w pos gsbind [ gen0, $gsae $ gsbcarg_w $gshere $ \ env0 ->
    gsbcapparg_w pos gsbind [ $gsae $ gen1 env0, $gsae $ gsbcarg_w $gshere $ \ env1 ->
    gsbcimplementationfailure_w $gshere $ "gsbccomposegen_w next"
  ]]

gsbcexecgen_w :: Pos -> GSArg -> GSArg -> GSExpr
gsbcexecgen_w pos gsmap e = gsbcapparg_w pos gsmap [ $gsae $ gsbcimplementationfailure_w $gshere $ "gsbcexecgen_w next", e ]

gsbcvarbind_w :: Pos -> GSArg -> GSVar -> GSArg -> GSExpr
gsbcvarbind_w pos gsmap x e = gsbcapparg_w pos gsmap [ $gsae $ gsbcimplementationfailure_w $gshere $ "gsbcvarbind_w next", e ]

gsbcemptygen_w :: Pos -> GSArg -> GSExpr
gsbcemptygen_w pos gsunit = gsbcapparg_w pos gsunit [ $gsae $ gsbcimplementationfailure_w $gshere $ "gsbcemptygen_w next" ]

gsbcimplet = varE 'gsbcimplet_w `appE` gshere

gsbcimplet_w :: Pos -> GSArg -> GSBCImp GSValue
gsbcimplet_w pos a = GSBCImp $ \ _ -> gsprepare_w pos a

gsbcimpbind = varE 'gsbcimpbind_w `appE` gshere

gsbcimpbind_w :: Pos -> GSArg -> GSBCImp GSValue
gsbcimpbind_w pos (GSArgVar v) = GSBCImp $ \ t -> apiCall pos v t
gsbcimpbind_w pos0 (GSArgExpr pos1 e) = GSBCImp $ \ t -> apiCallExpr pos0 e t
gsbcimpbind_w pos a = GSBCImp $ \t -> $apiImplementationFailure $ "gsbcimpbind_w " ++ argCode a ++ " next"

gsbcimpbody = varE 'gsbcimpbody_w `appE` gshere

gsbcimpbody_w :: Pos -> GSArg -> GSBCImp GSValue
gsbcimpbody_w pos0 (GSArgExpr pos1 e) = GSBCImp $ \ t -> apiCallExpr pos0 e t
gsbcimpbody_w pos a = GSBCImp $ \t -> $apiImplementationFailure $ "gsbcimpbody_w " ++ argCode a ++ " next"

gsbcimpunit_w :: Pos -> GSArg -> GSBCImp GSValue
gsbcimpunit_w pos a = GSBCImp $ \ t -> gsprepare_w pos a

gsbccomposeimpgen_w :: Pos -> GSArg -> (GSValue -> GSExpr) -> GSExpr
gsbccomposeimpgen_w pos gen0 gen1 = gsbcimpfor_w pos $ do
    env0 <- gsbcimpbind_w $gshere $ gen0
    env1 <- gsbcimpbind_w $gshere $ $gsae $ gen1 env0
    gsbcimpunit_w $gshere $ $gsae $ gsbcprim_w $gshere gsmergeenv env0 env1

gsbcimpexecbind_w :: Pos -> GSArg -> GSExpr
gsbcimpexecbind_w pos a = gsbcimpfor_w pos $ do
    gsbcimpbind_w $gshere a
    gsbcimpbody_w $gshere $ $gsae $ gsbcimplementationfailure_w $gshere "gsbcimpexecbind_w next"

gsbcimpvarbind_w :: Pos -> GSVar -> GSArg -> GSExpr
gsbcimpvarbind_w pos x a = gsbcimpfor_w pos $ do
    v <- gsbcimpbind_w $gshere a
    gsbcimpunit_w $gshere $ $gsav $ GSRecord pos (Map.fromList [(x, v)])

gsbcemptyimpgen_w :: Pos -> GSExpr
gsbcemptyimpgen_w pos = gsbcimpfor_w pos $ do
    gsbcimpunit_w $gshere $ $gsav $ GSRecord pos (Map.fromList [])

gsbcconstr_view = varE 'gsbcconstr_view_w `appE` gshere

gsbcconstr_view_w pos = gsbcconstr_view_ww pos . gsvar

gsbcconstr_view_ww :: Pos -> GSVar -> GSValue -> GSValue -> GSValue -> GSExpr
gsbcconstr_view_ww pos c ek sk x = gsbcforce_w pos (GSArgVar x) $ \ x0 -> case x0 of
        GSConstr pos1 c' as
            | c == c' -> gsbcapply_w pos sk (map GSArgVar as)
            | otherwise -> gsbcenter_w pos ek
        _ -> gsbcimplementationfailure_w $gshere $ "gsbcconstr_view_ww " ++ gsvCode x0 ++ " next"

gsbcviewpattern = varE 'gsbcviewpattern_w `appE` gshere

gsbcviewpattern_w :: (ToGSViewPattern res) => Pos -> GSValue -> res
gsbcviewpattern_w pos v =
    gsbcviewpattern_ww pos (\ sk -> gsbcapply_w pos v [ GSArgVar (GSConstr pos (gsvar "0") []), GSArgExpr pos (gsbcapp_w $gshere sk [GSArgVar $ GSConstr pos (gsvar "1") [GSRecord pos Map.empty]]) ])

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
gsbcvarpattern_ww pos v = gsbcarg_w pos $ \ x -> GSExpr $ \  st cs ->
    aceReturn (GSConstr pos (gsvar "1") [GSRecord pos $ Map.fromList [(v, x)]]) st

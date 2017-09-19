{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module GSI.ByteCode (
    gsbcundefined, gsbcundefined_w, gsbcarg, gsbcarg_w, gsbcenter, gsbcenter_w, gsbcapply, gsbcapply_w, gsbcprim, gsbcprim_w, gsbcimpprim, gsbcimpprim_w, gsbcforce, gsbcforce_w, gsbclfield, gsbclfield_w, gsbcfield, gsbcfield_w, gsbcrecord, gsbcrecord_w, gsbcconstr, gsbcconstr_w, gsbcexternal, gsbcexternal_w, gsbcchar_w, gsbchere, gsbchere_w, gsbcerror, gsbcerror_w, gsbcimplementationfailure, gsbcimplementationfailure_w,
    gsbccomposegen_w, gsbcvarmatch_w, gsbcemptygen_w,
    gsbccomposemonadgen_w, gsbcexecgen_w, gsbcvarbind_w, gsbcemptymonadgen_w,
    gsbcevalnatural, gsbcevalnatural_w, gsbcfmterrormsg, gsbcfmterrormsg_w,
    gsbcimpfor, gsbcimplet, gsbcimplet_w, gsbcimpbind, gsbcimpbind_w, gsbcimpbody, gsbcimpbody_w, gsbcimpunit, gsbcimpunit_w,
    gsbccomposeimpgen_w, gsbcimpexecbind_w, gsbcimpvarbind_w, gsbcemptyimpgen_w,
    gsbcconstr_view, gsbcconstr_view_w, gsbcconstr_view_ww,
    gsbcviewpattern, gsbcviewpattern_w, gsbcvarpattern, gsbcvarpattern_w, gsbcdiscardpattern, gsbcdiscardpattern_w
  ) where

import Control.Monad (forM)

import qualified Data.Map as Map

import Language.Haskell.TH.Lib (appE, varE)

import GSI.Util (Pos, StackTrace(..), gsfatal, gshere, fmtPos, filename, line, col)
import GSI.Syn (GSVar, gsvar, fmtVarAtom)
import GSI.Error (GSError(..))
import GSI.Value (GSValue(..), GSBCO(..), GSExpr(..), GSExternal(..), GSArg(..), GSStackFrame(..), GSBCImp(..), gsimplementationfailure, gsundefined_value_w, gslambda_value, gslambda_w, gsprepare_w, gsthunk_w, gsfield_w, gsimpfor_w, gsae, gsav, gsvCode, argCode)
import GSI.Functions (gsstring, gsnatural, gsfmterrormsg)
import GSI.ThreadType (Thread)
import GSI.CalculusPrims (gsparand, gsmergeenv)
import ACE (aceEnter, aceEnterExpr, aceForce, aceArg, aceReturn, aceThrow)
import API (apiCall, apiCallExpr, apiImplementationFailure)

gsbcundefined = varE 'gsbcundefined_w `appE` gshere

gsbcundefined_w :: Pos -> GSExpr
gsbcundefined_w pos = GSExpr $ \ st cs -> aceThrow (GSError (GSErrUnimpl (StackTrace pos cs))) st

gsbchere = varE 'gsbchere_w `appE` gshere

gsbchere_w :: Pos -> GSExpr
gsbchere_w pos = GSExpr $ \ st cs -> aceReturn (GSExternal (toExternal pos)) st

gsbcrecord = varE 'gsbcrecord_w `appE` gshere

gsbcrecord_w :: Pos -> [(GSVar, GSArg)] -> GSExpr
gsbcrecord_w pos fs = GSExpr $ \ st cs sk -> do
    fs' <- forM fs $ \ (v, fa) -> do
        fv <- gsprepare_w pos fa
        return (v, fv)
    aceReturn (GSRecord pos $ Map.fromList fs') st sk

gsbcconstr = varE 'gsbcconstr_w `appE` gshere

gsbcconstr_w :: Pos -> GSVar -> [GSArg] -> GSExpr
gsbcconstr_w pos c as = GSExpr $ \ st cs sk -> do
    asv <- mapM (gsprepare_w pos) as
    aceReturn (GSConstr pos c asv) st sk

gsbcexternal = varE 'gsbcexternal_w `appE` gshere

gsbcexternal_w :: GSExternal e => Pos -> e -> GSExpr
gsbcexternal_w pos e = GSExpr $ \ st cs -> aceReturn (GSExternal (toExternal e)) st

gsbcchar_w :: Pos -> Char -> GSExpr
gsbcchar_w pos ch = GSExpr $ \ st cs -> aceReturn (GSRune ch) st

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
gsbcapply_w pos f args = GSExpr $ \ st cs sk -> do
    asv <- mapM (gsprepare_w pos) args
    let (st', sk') = foldr (\ v (st0, sk0) -> ([], aceArg (StackTrace pos cs) v st0 sk0)) (st, sk) asv
    aceEnter [ StackTrace pos cs ] f st' sk'

gsbcapp_w :: Pos -> GSExpr -> [GSArg] -> GSExpr
gsbcapp_w pos f args = GSExpr $ \ st cs sk -> do
    asv <- mapM (gsprepare_w pos) args
    let (st', sk') = foldr (\ v (st0, sk0) -> ([], aceArg (StackTrace pos cs) v st0 sk0)) (st, sk) asv
    aceEnterExpr [StackTrace pos cs] f st' sk'

gsbcapparg_w :: Pos -> GSArg -> [GSArg] -> GSExpr
gsbcapparg_w pos (GSArgVar f) as = gsbcapply_w pos f as
gsbcapparg_w pos f as = gsbcimplementationfailure_w $gshere $ "gsbcapparg_w pos " ++ argCode f ++ " as next"

gsbcprim = varE 'gsbcprim_w `appE` gshere

class GSBCPrimType f r where
    gsbcprim_ww :: Pos -> f -> r

instance GSBCPrimType (IO GSValue) GSExpr where
    gsbcprim_ww pos f = GSExpr $ \ st cs sk -> do
        v <- f
        aceEnter [ StackTrace pos cs ] v st sk

instance GSBCPrimType f r => GSBCPrimType (GSValue -> f) (GSValue -> r) where
    gsbcprim_ww pos f v = gsbcprim_ww pos (f v)

gsbcprim_w :: GSBCPrimType f r => Pos -> (Pos -> f) -> r
gsbcprim_w pos f = gsbcprim_ww pos (f pos)

gsbcimpprim = varE 'gsbcimpprim_w `appE` gshere

class GSBCImpPrimType f r where
    gsbcimpprim_ww :: Pos -> (Thread -> f) -> r

instance GSBCImpPrimType (IO GSValue) GSExpr where
    gsbcimpprim_ww pos f = GSExpr $ \ st cs -> aceReturn (GSClosure [StackTrace pos []] (GSImp f)) st

instance GSBCImpPrimType f r => GSBCImpPrimType (GSValue -> f) (GSValue -> r) where
    gsbcimpprim_ww pos f v = gsbcimpprim_ww pos (\ t -> f t v)

gsbcimpprim_w :: GSBCImpPrimType f r => Pos -> (Pos -> Thread -> f) -> r
gsbcimpprim_w pos f = gsbcimpprim_ww pos (f pos)

gsbcforce = varE 'gsbcforce_w `appE` gshere

gsbcforce_w :: Pos -> GSArg -> (GSValue -> GSExpr) -> GSExpr
gsbcforce_w pos e k = GSExpr $ \ st cs sk -> let c1 = StackTrace pos cs in case e of
    GSArgExpr pos' e' -> aceEnterExpr [c1] e' [] (aceForce c1 k st sk)
    GSArgVar v -> aceEnter [c1] v [] (aceForce c1 k st sk)
    _ -> aceThrow ($gsimplementationfailure $ "gsbcforce_w " ++ argCode e ++ " next") st sk

gsbclet_w :: Pos -> GSExpr -> (GSValue -> GSExpr) -> GSExpr
gsbclet_w pos e k = GSExpr $ \ st cs sk -> do
    v <- gsthunk_w pos e
    aceEnterExpr [StackTrace pos cs] (k v) st sk

gsbcimpfor = varE 'gsbcimpfor_w `appE` gshere

gsbcimpfor_w :: Pos -> GSBCImp GSValue -> GSExpr
gsbcimpfor_w pos a = GSExpr $ \ st cs -> aceReturn (gsimpfor_w pos a) st

gsbclfield = varE 'gsbclfield_w `appE` gshere

gsbclfield_w :: Pos -> GSVar -> GSValue -> (GSValue -> GSExpr) -> GSExpr
gsbclfield_w pos f r k = GSExpr $ \ st cs sk -> do
    v <- gsfield_w pos f r
    aceEnterExpr [StackTrace pos cs] (k v) st sk

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
gsbcfmterrormsg_w pos msg k = GSExpr $ \ st cs sk -> do
    msgv <- gsprepare_w pos msg
    msgs <- $gsfmterrormsg msgv
    aceEnterExpr [StackTrace pos cs] (k msgs) st sk

gsbccomposegen_w :: Pos -> GSArg -> (GSValue -> GSExpr) -> GSExpr
gsbccomposegen_w pos gen0 gen1 = gsbcforce_w pos gen0 $ \ env0 -> gsbcforce_w pos ($gsae $ gen1 env0) $ \ env1 ->
    gsbcprim_w $gshere gsmergeenv env0 env1

gsbcvarmatch_w :: Pos -> GSVar -> GSArg -> GSExpr
gsbcvarmatch_w pos v e = gsbcrecord_w pos [(v, e)]

gsbcemptygen_w :: Pos -> GSExpr
gsbcemptygen_w pos = gsbcrecord_w pos []

gsbccomposemonadgen_w :: Pos -> GSArg -> GSArg -> GSArg -> (GSValue -> GSExpr) -> GSExpr
gsbccomposemonadgen_w pos gsbind gsunit gen0 gen1 = gsbcapparg_w pos gsbind [ gen0, $gsae $ gsbcarg_w $gshere $ \ env0 ->
    gsbcapparg_w pos gsbind [ $gsae $ gen1 env0, $gsae $ gsbcarg_w $gshere $ \ env1 ->
        gsbcapparg_w pos gsunit [ $gsae $ gsbcprim_w $gshere gsmergeenv env0 env1 ]
  ]]

gsbcexecgen_w :: Pos -> GSArg -> GSArg -> GSExpr
gsbcexecgen_w pos gsmap e = gsbcapparg_w pos gsmap [ $gsae $ gsbcarg_w $gshere $ \ _ -> gsbcrecord_w pos [], e ]

gsbcvarbind_w :: Pos -> GSArg -> GSVar -> GSArg -> GSExpr
gsbcvarbind_w pos gsmap x e = gsbcapparg_w pos gsmap [ $gsae $ gsbcarg_w $gshere $ \ v -> gsbcrecord_w pos [(x, $gsav v)], e ]

gsbcemptymonadgen_w :: Pos -> GSArg -> GSExpr
gsbcemptymonadgen_w pos gsunit = gsbcapparg_w pos gsunit [ $gsae $ gsbcrecord_w pos [] ]

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

gsbcimpunit = varE 'gsbcimpunit_w `appE` gshere

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
    gsbcimpunit_w $gshere $ $gsav $ GSRecord pos Map.empty

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

gsbcdiscardpattern = varE 'gsbcdiscardpattern_w `appE` gshere

gsbcdiscardpattern_w :: Pos -> GSExpr
gsbcdiscardpattern_w pos = gsbcarg_w pos $ \ x -> GSExpr $ \ st cs ->
    aceReturn (GSConstr pos (gsvar "1") [GSRecord pos Map.empty]) st

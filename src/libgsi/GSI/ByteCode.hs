{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module GSI.ByteCode (
    gsbcundefined, gsbcundefined_w, gsbcarg, gsbcarg_w, gsbcenter, gsbcenter_w, gsbcapply, gsbcapply_w, gsbcapp_w, gsbcprim, gsbcprim_w, gsbcimpprim, gsbcimpprim_w, gsbcforce, gsbcforce_w, gsbclfield, gsbclfield_w, gsbcfield, gsbcfield_w, gsbcevalexternal, gsbcevalexternal_w, gsbcnatural, gsbcnatural_w, gsbcrecord, gsbcrecord_w, gsbcconstr, gsbcconstr_w, gsbcexternal, gsbcexternal_w, gsbcchar_w, gsbcwithhere, gsbcwithhere_w, gsbcerror, gsbcimplementationfailure, gsbcimplementationfailure_w,
    gsbccomposegen_w, gsbcvarmatch_w, gsbcemptygen_w,
    gsbccomposemonadgen_w, gsbcexecgen_w, gsbcvarbind_w, gsbcemptymonadgen_w,
    gsbcevalnatural, gsbcevalnatural_w, gsbcfmterrormsg, gsbcfmterrormsg_w,
    gsbcimpfor, gsbcimplet, gsbcimplet_w, gsbcimpbind, gsbcimpbind_w, gsbcimpbody, gsbcimpbody_w, gsbcimpunit, gsbcimpunit_w,
    gsbccomposeimpgen_w, gsbcimpexecbind_w, gsbcimpvarbind_w, gsbcemptyimpgen_w,
    gsbcconstr_view, gsbcconstr_view_w, gsbcconstr_view_ww,
    gsbcviewpattern_w, gsbcnonmonoidalpattern_w, gsbcvarpattern_w, gsbcdiscardpattern_w
  ) where

import Control.Monad (forM)

import qualified Data.Map as Map

import Language.Haskell.TH.Lib (appE, varE)

import GSI.Util (Pos, StackTrace(..), gsfatal, gshere, fmtPos, filename, line, col)
import GSI.Syn (GSVar, gsvar, fmtVarAtom)
import GSI.Error (GSError(..))
import GSI.Value (GSValue(..), GSBCO(..), GSExpr(..), GSExprCont(..), GSExternal(..), GSArg(..), GSBCImp(..), gsimplementationfailure, gsundefined_value_w, gslambda_value, gslambda_w, gsprepare_w, gsthunk_w, gsfield_w, gsimpfor_w, gsexternal, whichExternal, gsae, gsav, gsvCode, argCode)
import GSI.Functions (gsstring, gsnatural, gsfmterrormsg)
import GSI.ThreadType (Thread)
import GSI.CalculusPrims (gsparand, gsmergeenv)
import ACE (aceEnter, aceForce, aceArg, aceField)
import API (apiCall, apiCallExpr, apiImplementationFailure)

gsbcundefined :: StackTrace -> GSExpr
gsbcundefined st = GSExpr $ \ _ sk -> gsthrow sk $ GSError (GSErrUnimpl st)

gsbcundefined_w :: Pos -> GSExpr
gsbcundefined_w pos = GSExpr $ \ cs sk -> gsthrow sk $ GSError $ GSErrUnimpl $ StackTrace pos cs

gsbcwithhere = varE 'gsbcwithhere_w `appE` gshere

gsbcwithhere_w :: Pos -> (GSValue -> GSExpr) -> GSExpr
gsbcwithhere_w pos k = GSExpr $ \ cs sk -> runGSExpr (k $ gsexternal $ StackTrace pos cs) cs sk

gsbcnatural = varE 'gsbcnatural_w `appE` gshere

gsbcnatural_w :: Pos -> Integer -> GSExpr
gsbcnatural_w pos n = GSExpr $ \ cs sk -> gsreturn sk $ GSNatural n

gsbcrecord = varE 'gsbcrecord_w `appE` gshere

gsbcrecord_w :: Pos -> [(GSVar, GSArg)] -> GSExpr
gsbcrecord_w pos fs = GSExpr $ \ cs sk -> do
    fs' <- forM fs $ \ (v, fa) -> do
        fv <- gsprepare_w pos fa
        return (v, fv)
    gsreturn sk $ GSRecord pos $ Map.fromList fs'

gsbcconstr = varE 'gsbcconstr_w `appE` gshere

gsbcconstr_w :: Pos -> GSVar -> [GSArg] -> GSExpr
gsbcconstr_w pos c as = GSExpr $ \ cs sk -> do
    asv <- mapM (gsprepare_w pos) as
    gsreturn sk $ GSConstr pos c asv

gsbcexternal = varE 'gsbcexternal_w `appE` gshere

gsbcexternal_w :: GSExternal e => Pos -> e -> GSExpr
gsbcexternal_w pos e = GSExpr $ \ cs sk -> gsreturn sk $ GSExternal (toExternal e)

gsbcchar_w :: Pos -> Char -> GSExpr
gsbcchar_w pos ch = GSExpr $ \ cs sk -> gsreturn sk $ GSRune ch

gsbcerror :: StackTrace -> String -> GSExpr
gsbcerror (StackTrace pos _) msg = GSExpr $ \ _ sk -> gsthrow sk $ GSError (GSErrError pos msg)

gsbcimplementationfailure = varE 'gsbcimplementationfailure_w `appE` gshere

gsbcimplementationfailure_w :: Pos -> String -> GSExpr
gsbcimplementationfailure_w pos msg = GSExpr $ \ cs sk -> gsthrow sk $ GSImplementationFailure pos msg

gsbcarg = varE 'gsbcarg_w `appE` gshere

gsbcarg_w :: Pos -> (GSValue -> GSExpr) -> GSExpr
gsbcarg_w pos fn = GSExpr $ \ cs sk -> do
    aceEnter [ StackTrace pos cs ] (gslambda_w pos fn) sk

gsbcenter = varE 'gsbcenter_w `appE` gshere

gsbcenter_w :: Pos -> GSValue -> GSExpr
gsbcenter_w pos v = GSExpr $ \ cs sk -> aceEnter [ StackTrace pos cs ] v sk

gsbcapply = varE 'gsbcapply_w `appE` gshere

gsbcapply_w :: Pos -> GSValue -> [GSArg] -> GSExpr
gsbcapply_w pos f args = GSExpr $ \ cs sk -> do
    asv <- mapM (gsprepare_w pos) args
    aceEnter [ StackTrace pos cs ] f (foldr (\ v -> aceArg (StackTrace pos cs) v) sk asv)

gsbcapp_w :: Pos -> GSExpr -> [GSArg] -> GSExpr
gsbcapp_w pos f args = GSExpr $ \ cs sk -> do
    asv <- mapM (gsprepare_w pos) args
    runGSExpr f [StackTrace pos cs] (foldr (\ v -> aceArg (StackTrace pos cs) v) sk asv)

gsbcapparg_w :: Pos -> GSArg -> [GSArg] -> GSExpr
gsbcapparg_w pos (GSArgVar f) as = gsbcapply_w pos f as
gsbcapparg_w pos (GSArgExpr pos1 e) as = gsbcapp_w pos1 e as
gsbcapparg_w pos f as = gsbcimplementationfailure_w $gshere $ "gsbcapparg_w pos " ++ argCode f ++ " as next"

gsbcprim = varE 'gsbcprim_w `appE` gshere

class GSBCPrimType f r where
    gsbcprim_ww :: Pos -> f -> r

instance GSBCPrimType (IO GSValue) GSExpr where
    gsbcprim_ww pos f = GSExpr $ \ cs sk -> do
        v <- f
        aceEnter [ StackTrace pos cs ] v sk

instance GSBCPrimType f r => GSBCPrimType (GSValue -> f) (GSValue -> r) where
    gsbcprim_ww pos f v = gsbcprim_ww pos (f v)

gsbcprim_w :: GSBCPrimType f r => Pos -> (Pos -> f) -> r
gsbcprim_w pos f = gsbcprim_ww pos (f pos)

gsbcimpprim = varE 'gsbcimpprim_w `appE` gshere

class GSBCImpPrimType f r where
    gsbcimpprim_ww :: Pos -> (Thread -> f) -> r

instance GSBCImpPrimType (IO GSValue) GSExpr where
    gsbcimpprim_ww pos f = GSExpr $ \ cs sk -> gsreturn sk $ GSClosure [StackTrace pos []] (GSImp f)

instance GSBCImpPrimType f r => GSBCImpPrimType (GSValue -> f) (GSValue -> r) where
    gsbcimpprim_ww pos f v = gsbcimpprim_ww pos (\ t -> f t v)

gsbcimpprim_w :: GSBCImpPrimType f r => Pos -> (Pos -> Thread -> f) -> r
gsbcimpprim_w pos f = gsbcimpprim_ww pos (f pos)

gsbcforce = varE 'gsbcforce_w `appE` gshere

gsbcforce_w :: Pos -> GSArg -> (GSValue -> GSExpr) -> GSExpr
gsbcforce_w pos e k = GSExpr $ \ cs sk -> let c1 = StackTrace pos cs in runGSArg c1 e (aceForce c1 k sk)

runGSArg :: StackTrace -> GSArg ->  GSExprCont a -> IO a
runGSArg c1 (GSArgExpr pos' e') sk = runGSExpr e' [c1] sk
runGSArg c1 (GSArgVar v) sk = aceEnter [c1] v sk
runGSArg c1 a sk = gsthrow sk $ $gsimplementationfailure $ "runGSArg " ++ argCode a ++ " next"

gsbclet_w :: Pos -> GSExpr -> (GSValue -> GSExpr) -> GSExpr
gsbclet_w pos e k = GSExpr $ \ cs sk -> do
    v <- gsthunk_w pos e
    runGSExpr (k v) [StackTrace pos cs] sk

gsbcimpfor = varE 'gsbcimpfor_w `appE` gshere

gsbcimpfor_w :: Pos -> GSBCImp GSValue -> GSExpr
gsbcimpfor_w pos a = GSExpr $ \ cs sk -> gsreturn sk $ gsimpfor_w pos a

gsbclfield = varE 'gsbclfield_w `appE` gshere

gsbclfield_w :: Pos -> GSVar -> GSValue -> (GSValue -> GSExpr) -> GSExpr
gsbclfield_w pos f r k = GSExpr $ \ cs sk -> do
    v <- gsfield_w pos f r
    runGSExpr (k v) [StackTrace pos cs] sk

gsbcfield = varE 'gsbcfield_w `appE` gshere

gsbcfield_w :: Pos -> GSArg -> GSVar -> GSExpr
gsbcfield_w pos a f = GSExpr $ \ cs sk -> let c1 = StackTrace pos cs in runGSArg c1 a (aceField c1 f sk)

gsbcevalexternal = varE 'gsbcevalexternal_w `appE` gshere

gsbcevalexternal_w :: GSExternal e => Pos -> GSArg -> (e -> GSExpr) -> GSExpr
gsbcevalexternal_w pos ea k = gsbcforce_w pos ea $ \ ev -> case ev of
    GSExternal e -> case fromExternal e of
        Just x -> k x
        Nothing -> gsbcimplementationfailure_w $gshere $ "gsbcevalexternal_w " ++ whichExternal e ++ " next"
    _ -> gsbcimplementationfailure_w $gshere $ "gsbcevalexternal_w " ++ gsvCode ev ++ " next"

gsbcevalnatural = varE 'gsbcevalnatural_w `appE` gshere

gsbcevalnatural_w :: Pos -> GSArg -> (Integer -> GSExpr) -> GSExpr
gsbcevalnatural_w pos na k = gsbcforce_w pos na $ \ nv -> case nv of
    GSNatural n -> k n
    _ -> gsbcimplementationfailure_w $gshere $ "gsbcevalnatural_w " ++ gsvCode nv ++ " next"

gsbcfmterrormsg = varE 'gsbcfmterrormsg_w `appE` gshere

gsbcfmterrormsg_w :: Pos -> GSArg -> (String -> GSExpr) -> GSExpr
gsbcfmterrormsg_w pos msg k = GSExpr $ \ cs sk -> do
    msgv <- gsprepare_w pos msg
    msgs <- $gsfmterrormsg msgv
    runGSExpr (k msgs) cs sk

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

gsbcviewpattern_w :: Pos -> GSValue -> [GSArg] -> GSExpr
gsbcviewpattern_w pos v ps = gsbcarg_w $gshere $ \ x -> gsbcapply_w $gshere v [
    $gsav (GSConstr $gshere (gsvar "0") []),
    $gsae $ (foldr
        (\ p k r -> gsbcarg_w $gshere $ \ x0 ->
            k $ gsbclet_w $gshere r $ \ rv -> gsbclet_w $gshere (gsbcapparg_w $gshere p [ $gsav x0 ]) $ \ px0 ->
                (gsbcprim_w $gshere gsparand rv px0)
        )
        (\ r -> r)
        ps
        (gsbcconstr_w $gshere (gsvar "1") [ $gsae $ gsbcrecord_w $gshere []])
    ),
    $gsav x
  ]

gsbcnonmonoidalpattern_w :: Pos -> GSExpr -> GSExpr
gsbcnonmonoidalpattern_w pos p = gsbcarg_w pos $ \ x ->
    gsbcforce_w pos ($gsae (gsbcapp_w pos p [ $gsav x ])) $ \ r -> gsbcconstr_w pos (gsvar "1") [$gsav r]

gsbcvarpattern_w :: Pos -> GSVar -> GSExpr
gsbcvarpattern_w pos v = gsbcarg_w pos $ \ x -> GSExpr $ \ cs sk ->
    gsreturn sk (GSRecord pos $ Map.fromList [(v, x)])

gsbcdiscardpattern_w :: Pos -> GSExpr
gsbcdiscardpattern_w pos = gsbcarg_w pos $ \ x -> GSExpr $ \ cs sk ->
    gsreturn sk (GSRecord pos Map.empty)

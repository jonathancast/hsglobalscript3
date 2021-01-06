{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module GSI.ByteCode (
    gsbcundefined, gsbcundefined_w, gsbcarg, gsbcarg_w, gsbcenter, gsbcenter_w, gsbcenterarg, gsbcenterarg_w, gsbcapply, gsbcapply_w, gsbcapp, gsbcapp_w, gsbcprim, gsbcprim_w, gsbcimpprim, gsbcimpprim_w, gsbcforce, gsbcforce_w, gsbclfield, gsbclfield_w, gsbcfield, gsbcfield_w, gsbcevalexternal, gsbcevalexternal_w, gsbcrune, gsbcrune_w, gsbcnatural, gsbcnatural_w, gsbcbool, gsbcbool_w, gsbcrecord, gsbcrecord_w, gsbcconstr, gsbcconstr_w, gsbcexternal, gsbcexternal_w, gsbcchar_w, gsbcrehere_w, gsbcat_w, gsbcerror, gsbcruntimetypeerror, gsbcruntimetypeerror_w, gsbcimplementationfailure, gsbcimplementationfailure_w,
    gsbccomposegen_w, gsbcvarmatch_w, gsbcemptygen_w,
    gsbccomposemonadgen_w, gsbcexecgen_w, gsbcvarbind_w, gsbcemptymonadgen_w,
    gsbcevalnatural, gsbcevalnatural_w, gsbcfmterrormsg, gsbcfmterrormsg_w,
    gsbcimpfor, gsbcimplet, gsbcimplet_w, gsbcimpbind, gsbcimpbind_w, gsbcimpbody, gsbcimpbody_w, gsbcimpunit, gsbcimpunit_w,
    gsbccomposeimpgen_w, gsbcimpexecbind_w, gsbcimpvarbind_w, gsbcemptyimpgen_w,
    gsbcconstr_view, gsbcconstr_view_w,
    gsbcviewpattern_w, gsbcvarpattern_w, gsbcrunepattern_w, gsbcdiscardpattern_w
  ) where

import Data.Proxy (Proxy(..))

import Control.Monad (forM)

import qualified Data.Map as Map

import Language.Haskell.TH.Lib (appE, varE)

import GSI.Util (Pos, StackTrace(..), gsfatal, gshere, fmtPos, fmtCallers, filename, line, col)
import GSI.Syn (GSVar, gsvar, fmtVarAtom)
import GSI.Error (GSError(..), GSInvalidProgram(..))
import GSI.Message (Message)
import GSI.Prof (ProfCounter, prof)
import GSI.RTS (OPort)
import GSI.Value (GSValue(..), GSBCO(..), GSExpr(..), GSIntExpr, GSExprCont(..), GSExternal(..), GSArg(..), GSBCImp(..), gsimplementationfailure, gsundefined_value_w, gslambda_value, gslambda_w, gsprepare_w, gsthunk_w, gsfield_w, gsimpfor_w, gsexternal, whichExternal, gsae, gsav, gsvCode, argCode)
import GSI.Functions (gsfmterrormsg)
import GSI.ThreadType (Thread)
import GSI.CalculusPrims (gsparand, gsmergeenv)
import ACE (aceEnter, aceEnterIntExpr, aceForce, aceArg, aceField, aceReHere)
import API (apiCall, apiCallExpr, apiImplementationFailure)

gsbcundefined :: StackTrace -> GSExpr
gsbcundefined st = GSExpr $ \ _ _ _ sk -> gsthrow sk $ GSError (GSErrUnimpl st)

gsbcundefined_w :: Pos -> GSExpr
gsbcundefined_w pos = gsbcprof_w pos $ GSExpr $ \ msg pc cs sk -> gsthrow sk $ GSError $ GSErrUnimpl $ StackTrace pos cs

gsbcrehere_w :: Pos -> GSExpr -> GSExpr
gsbcrehere_w pos e = gsbcprof_w pos $ GSExpr $ \ msg pc cs sk -> runGSExpr e msg pc cs $ aceReHere pos cs sk

gsbcat_w :: Pos -> [StackTrace] -> GSExpr -> GSExpr
gsbcat_w pos cs k = gsbcprof_w pos $ GSExpr $ \ msg pc _ sk -> runGSExpr k msg pc cs sk

gsbcrune = varE 'gsbcrune_w `appE` gshere

gsbcrune_w :: Pos -> Char -> GSExpr
gsbcrune_w pos r = gsbcprof_w pos $ GSExpr $ \ msg pc cs sk -> gsreturn sk $ GSRune r

gsbcnatural = varE 'gsbcnatural_w `appE` gshere

gsbcnatural_w :: Pos -> Integer -> GSExpr
gsbcnatural_w pos n = gsbcprof_w pos $ GSExpr $ \ msg pc cs sk -> gsreturn sk $ GSNatural n

gsbcrecord = varE 'gsbcrecord_w `appE` gshere

gsbcrecord_w :: Pos -> [(GSVar, GSArg)] -> GSExpr
gsbcrecord_w pos fs = gsbcprof_w pos $ GSExpr $ \ msg pc cs sk -> do
    fs' <- forM fs $ \ (v, fa) -> do
        fv <- gsprepare_w pos fa
        return (v, fv)
    gsreturn sk $ GSRecord pos $ Map.fromList fs'

gsbcbool = varE 'gsbcbool_w `appE` gshere
gsbcbool_w pos b = gsbcprof_w pos $ case b of
    True -> gsbcconstr_w pos (gsvar "true") []
    False -> gsbcconstr_w pos (gsvar "false") []

gsbcconstr = varE 'gsbcconstr_w `appE` gshere

gsbcconstr_w :: Pos -> GSVar -> [GSArg] -> GSExpr
gsbcconstr_w pos c as = gsbcprof_w pos $ GSExpr $ \ msg pc cs sk -> do
    asv <- mapM (gsprepare_w pos) as
    gsreturn sk $ GSConstr pos c asv

gsbcexternal = varE 'gsbcexternal_w `appE` gshere

gsbcexternal_w :: GSExternal e => Pos -> e -> GSExpr
gsbcexternal_w pos e = gsbcprof_w pos $ GSExpr $ \ msg pc cs sk -> gsreturn sk $ GSExternal (toExternal e)

gsbcchar_w :: Pos -> Char -> GSExpr
gsbcchar_w pos ch = gsbcprof_w pos $ GSExpr $ \ msg pc cs sk -> gsreturn sk $ GSRune ch

gsbcerror = varE 'gsbcerror_w `appE` gshere

gsbcerror_w :: Pos -> String -> GSExpr
gsbcerror_w pos msg = gsbcprof_w pos $ GSExpr $ \ _ _ _ sk -> gsthrow sk $ GSError (GSErrError pos msg)

gsbcruntimetypeerror = varE 'gsbcruntimetypeerror_w `appE` gshere
gsbcruntimetypeerror_w :: Pos -> String -> String -> String -> GSExpr
gsbcruntimetypeerror_w pos ctxt act exp = gsbcprof_w pos $ GSExpr $ \ msg pc cs sk -> gsthrow sk $ GSInvalidProgram $ GSIPRuntimeTypeError (StackTrace pos cs) ctxt act exp

gsbcimplementationfailure = varE 'gsbcimplementationfailure_w `appE` gshere

gsbcimplementationfailure_w :: Pos -> String -> GSExpr
gsbcimplementationfailure_w pos msgs = gsbcprof_w pos $ GSExpr $ \ msg pc cs sk -> gsthrow sk $ GSImplementationFailure pos msgs

gsbcarg = varE 'gsbcarg_w `appE` gshere

gsbcarg_w :: Pos -> (GSValue -> GSExpr) -> GSExpr
gsbcarg_w pos fn = gsbcprof_w pos $ GSExpr $ \ msg pc cs sk -> do
    aceEnter msg pc [ StackTrace pos cs ] (GSClosure [StackTrace pos cs] (GSLambda (GSRawExpr . fn))) sk

gsbcenter = varE 'gsbcenter_w `appE` gshere

gsbcenter_w :: Pos -> GSValue -> GSExpr
gsbcenter_w pos v = gsbcprof_w pos $ GSExpr $ \ msg pc cs sk -> aceEnter msg pc [ StackTrace pos cs ] v sk

gsbcenterarg = varE 'gsbcenterarg_w `appE` gshere

gsbcenterarg_w :: Pos -> GSArg -> GSExpr
gsbcenterarg_w pos (GSArgExpr pos1 e) = gsbcprof_w pos $ GSExpr $ \ msg pc cs sk -> runGSExpr e msg pc [ StackTrace pos1 [ StackTrace pos cs ] ] sk
gsbcenterarg_w pos (GSArgVar v) = gsbcprof_w pos $ gsbcenter_w pos v

gsbcapply = varE 'gsbcapply_w `appE` gshere

gsbcapply_w :: Pos -> GSValue -> [GSArg] -> GSExpr
gsbcapply_w pos f args = gsbcprof_w pos $ GSExpr $ \ msg pc cs sk -> do
    asv <- mapM (gsprepare_w pos) args
    aceEnter msg pc [ StackTrace pos cs ] f (aceArg msg pc (StackTrace pos cs) asv sk)

gsbcapp = varE 'gsbcapp_w `appE` gshere

gsbcapp_w :: Pos -> GSExpr -> [GSArg] -> GSExpr
gsbcapp_w pos f args = gsbcprof_w pos $ GSExpr $ \ msg pc cs sk -> do
    asv <- mapM (gsprepare_w pos) args
    runGSExpr f msg pc [StackTrace pos cs] (aceArg msg pc (StackTrace pos cs) asv sk)

gsbcapparg_w :: Pos -> GSArg -> [GSArg] -> GSExpr
gsbcapparg_w pos (GSArgVar f) as = gsbcapply_w pos f as
gsbcapparg_w pos (GSArgExpr pos1 e) as = gsbcapp_w pos1 e as
gsbcapparg_w pos f as = gsbcimplementationfailure_w $gshere $ "gsbcapparg_w pos " ++ argCode f ++ " as next"

gsbcprim = varE 'gsbcprim_w `appE` gshere

class GSBCPrimType f r where
    gsbcprim_ww :: Pos -> (OPort Message -> Maybe ProfCounter -> f) -> r

instance GSBCPrimType (IO GSValue) GSExpr where
    gsbcprim_ww pos f = gsbcprof_w pos $ GSExpr $ \ msg pc cs sk -> do
        v <- f msg pc
        aceEnter msg pc [ StackTrace pos cs ] v sk

instance GSBCPrimType f r => GSBCPrimType (GSValue -> f) (GSValue -> r) where
    gsbcprim_ww pos f v = gsbcprim_ww pos (\ msg pc -> f msg pc v)

gsbcprim_w :: GSBCPrimType f r => Pos -> (OPort Message -> Maybe ProfCounter -> Pos -> f) -> r
gsbcprim_w pos f = gsbcprim_ww pos (\ msg pc -> f msg pc pos)

gsbcimpprim = varE 'gsbcimpprim_w `appE` gshere

class GSBCImpPrimType f r where
    gsbcimpprim_ww :: Pos -> (OPort Message -> Maybe ProfCounter -> Thread -> f) -> r

instance GSBCImpPrimType (IO GSValue) GSExpr where
    gsbcimpprim_ww pos f = GSExpr $ \ msg pc cs sk -> gsreturn sk $ GSClosure [StackTrace pos []] (GSImp f)

instance GSBCImpPrimType f r => GSBCImpPrimType (GSValue -> f) (GSValue -> r) where
    gsbcimpprim_ww pos f v = gsbcimpprim_ww pos (\ msg pc t -> f msg pc t v)

gsbcimpprim_w :: GSBCImpPrimType f r => Pos -> (OPort Message -> Maybe ProfCounter -> Pos -> Thread -> f) -> r
gsbcimpprim_w pos f = gsbcimpprim_ww pos (\ msg pc -> f msg pc pos)

gsbcforce = varE 'gsbcforce_w `appE` gshere

gsbcforce_w :: Pos -> GSArg -> (GSValue -> GSExpr) -> GSExpr
gsbcforce_w pos e k = gsbcprof_w pos $ GSExpr $ \ msg pc cs sk -> let c1 = StackTrace pos cs in runGSArg msg pc c1 e (aceForce msg pc cs k sk)

runGSArg :: OPort Message -> Maybe ProfCounter -> StackTrace -> GSArg ->  GSExprCont a -> IO a
runGSArg msg pc c1 (GSArgExpr pos' e') sk = runGSExpr e' msg pc [c1] sk
runGSArg msg pc c1 (GSArgVar v) sk = aceEnter msg pc [c1] v sk
runGSArg msg pc c1 a sk = gsthrow sk $ $gsimplementationfailure $ "runGSArg " ++ argCode a ++ " next"

gsbclet_w :: Pos -> GSExpr -> (GSValue -> GSExpr) -> GSExpr
gsbclet_w pos e k = gsbcprof_w pos $ GSExpr $ \ msg pc cs sk -> do
    v <- gsthunk_w pos e
    runGSExpr (k v) msg pc [StackTrace pos cs] sk

gsbcimpfor = varE 'gsbcimpfor_w `appE` gshere

gsbcimpfor_w :: Pos -> GSBCImp GSValue -> GSExpr
gsbcimpfor_w pos a = gsbcprof_w pos $ GSExpr $ \ msg pc cs sk -> gsreturn sk $ gsimpfor_w pos a

gsbclfield = varE 'gsbclfield_w `appE` gshere

gsbclfield_w :: Pos -> GSVar -> GSValue -> (GSValue -> GSExpr) -> GSExpr
gsbclfield_w pos f r k = gsbcprof_w pos $ GSExpr $ \ msg pc cs sk -> do
    v <- gsfield_w pos f r
    runGSExpr (k v) msg pc [StackTrace pos cs] sk

gsbcfield = varE 'gsbcfield_w `appE` gshere

gsbcfield_w :: Pos -> GSArg -> GSVar -> GSExpr
gsbcfield_w pos a f = gsbcprof_w pos $ GSExpr $ \ msg pc cs sk -> let c1 = StackTrace pos cs in runGSArg msg pc c1 a (aceField msg pc c1 f sk)

gsbcevalexternal = varE 'gsbcevalexternal_w `appE` gshere

gsbcevalexternal_w :: forall e. GSExternal e => Pos -> GSArg -> (e -> GSExpr) -> GSExpr
gsbcevalexternal_w pos ea k = gsbcprof_w pos $ gsbcforce_w pos ea $ \ ev -> case ev of
    GSExternal e -> case fromExternal e of
        Just x -> k x
        Nothing -> gsbcruntimetypeerror_w pos (externalType (Proxy :: Proxy e)) (whichExternal e) "gsbcevalexternal"
    _ -> gsbcimplementationfailure_w $gshere $ "gsbcevalexternal_w " ++ gsvCode ev ++ " next"

gsbcevalnatural = varE 'gsbcevalnatural_w `appE` gshere

gsbcevalnatural_w :: Pos -> GSArg -> (Integer -> GSExpr) -> GSExpr
gsbcevalnatural_w pos na k = gsbcprof_w pos $ gsbcforce_w pos na $ \ nv -> case nv of
    GSNatural n -> k n
    _ -> gsbcimplementationfailure_w $gshere $ "gsbcevalnatural_w " ++ gsvCode nv ++ " next"

gsbcfmterrormsg = varE 'gsbcfmterrormsg_w `appE` gshere

gsbcfmterrormsg_w :: Pos -> GSArg -> (String -> GSExpr) -> GSExpr
gsbcfmterrormsg_w pos msga k = gsbcprof_w pos $ GSExpr $ \ msg pc cs sk -> do
    msgv <- gsprepare_w pos msga
    msgs <- $gsfmterrormsg msg msgv
    runGSExpr (k msgs) msg pc cs sk

gsbccomposegen_w :: Pos -> GSArg -> (GSValue -> GSExpr) -> GSExpr
gsbccomposegen_w pos gen0 gen1 = gsbcprof_w pos $ gsbcforce_w pos gen0 $ \ env0 -> gsbcforce_w pos ($gsae $ gen1 env0) $ \ env1 ->
    gsbcprim_w $gshere gsmergeenv env0 env1

gsbcvarmatch_w :: Pos -> GSVar -> GSArg -> GSExpr
gsbcvarmatch_w pos v e = gsbcprof_w pos $ gsbcrecord_w pos [(v, e)]

gsbcemptygen_w :: Pos -> GSExpr
gsbcemptygen_w pos = gsbcprof_w pos $ gsbcrecord_w pos []

gsbccomposemonadgen_w :: Pos -> GSArg -> GSArg -> GSArg -> (GSValue -> GSExpr) -> GSExpr
gsbccomposemonadgen_w pos gsbind gsunit gen0 gen1 = gsbcprof_w pos $ gsbcapparg_w pos gsbind [ gen0, $gsae $ gsbcarg_w $gshere $ \ env0 ->
    gsbcapparg_w pos gsbind [ $gsae $ gen1 env0, $gsae $ gsbcarg_w $gshere $ \ env1 ->
        gsbcapparg_w pos gsunit [ $gsae $ gsbcprim_w $gshere gsmergeenv env0 env1 ]
    ]
  ]

gsbcexecgen_w :: Pos -> GSArg -> GSArg -> GSExpr
gsbcexecgen_w pos gsmap e = gsbcprof_w pos $ gsbcapparg_w pos gsmap [ $gsae $ gsbcarg_w $gshere $ \ _ -> gsbcrecord_w pos [], e ]

gsbcvarbind_w :: Pos -> GSArg -> GSVar -> GSArg -> GSExpr
gsbcvarbind_w pos gsmap x e = gsbcprof_w pos $ gsbcapparg_w pos gsmap [ $gsae $ gsbcarg_w $gshere $ \ v -> gsbcrecord_w pos [(x, $gsav v)], e ]

gsbcemptymonadgen_w :: Pos -> GSArg -> GSExpr
gsbcemptymonadgen_w pos gsunit = gsbcprof_w pos $ gsbcapparg_w pos gsunit [ $gsae $ gsbcrecord_w pos [] ]

gsbcimplet = varE 'gsbcimplet_w `appE` gshere

gsbcimplet_w :: Pos -> GSArg -> GSBCImp GSValue
gsbcimplet_w pos a = GSBCImp $ \ _ _ _ -> gsprepare_w pos a

gsbcimpbind = varE 'gsbcimpbind_w `appE` gshere

gsbcimpbind_w :: Pos -> GSArg -> GSBCImp GSValue
gsbcimpbind_w pos (GSArgVar v) = GSBCImp $ \ msg pc t -> apiCall msg pc pos v t
gsbcimpbind_w pos0 (GSArgExpr pos1 e) = GSBCImp $ \ msg pc t -> apiCallExpr msg pc pos0 e t
gsbcimpbind_w pos a = GSBCImp $ \ msg pc t -> $apiImplementationFailure $ "gsbcimpbind_w " ++ argCode a ++ " next"

gsbcimpbody = varE 'gsbcimpbody_w `appE` gshere

gsbcimpbody_w :: Pos -> GSArg -> GSBCImp GSValue
gsbcimpbody_w pos0 (GSArgExpr pos1 e) = GSBCImp $ \ msg pc t -> apiCallExpr msg pc pos0 e t
gsbcimpbody_w pos a = GSBCImp $ \ msg pc t -> $apiImplementationFailure $ "gsbcimpbody_w " ++ argCode a ++ " next"

gsbcimpunit = varE 'gsbcimpunit_w `appE` gshere

gsbcimpunit_w :: Pos -> GSArg -> GSBCImp GSValue
gsbcimpunit_w pos a = GSBCImp $ \ msg pc t -> gsprepare_w pos a

gsbccomposeimpgen_w :: Pos -> GSArg -> (GSValue -> GSExpr) -> GSExpr
gsbccomposeimpgen_w pos gen0 gen1 = gsbcprof_w pos $ gsbcimpfor_w pos $ do
    env0 <- gsbcimpbind_w $gshere $ gen0
    env1 <- gsbcimpbind_w $gshere $ $gsae $ gen1 env0
    gsbcimpunit_w $gshere $ $gsae $ gsbcprim_w $gshere gsmergeenv env0 env1

gsbcimpexecbind_w :: Pos -> GSArg -> GSExpr
gsbcimpexecbind_w pos a = gsbcprof_w pos $ gsbcimpfor_w pos $ do
    gsbcimpbind_w $gshere a
    gsbcimpunit_w $gshere $ $gsav $ GSRecord pos Map.empty

gsbcimpvarbind_w :: Pos -> GSVar -> GSArg -> GSExpr
gsbcimpvarbind_w pos x a = gsbcprof_w pos $ gsbcimpfor_w pos $ do
    v <- gsbcimpbind_w $gshere a
    gsbcimpunit_w $gshere $ $gsav $ GSRecord pos (Map.fromList [(x, v)])

gsbcemptyimpgen_w :: Pos -> GSExpr
gsbcemptyimpgen_w pos = gsbcprof_w pos $ gsbcimpfor_w pos $ do
    gsbcimpunit_w $gshere $ $gsav $ GSRecord pos (Map.fromList [])

gsbcconstr_view = varE 'gsbcconstr_view_w `appE` gshere

gsbcconstr_view_w pos nm = gslambda_w pos $ \ ek -> gsbcarg_w pos $ \ sk -> gsbcarg_w pos $ \ x ->
    gsbcprof_w pos $ gsbcforce_w pos (GSArgVar x) $ \ x0 -> case x0 of
        GSConstr pos1 c as
            | c == gsvar nm -> gsbcapply_w pos sk (map GSArgVar as)
            | otherwise -> gsbcenter_w pos ek
        GSClosure cs _ -> gsbcruntimetypeerror_w pos ("view " ++ fmtVarAtom (gsvar nm) " ek sk •") ("GSClosure at " ++ fmtCallers cs "") "GSConstr"
        _ -> gsbcruntimetypeerror_w pos ("view " ++ fmtVarAtom (gsvar nm) " ek sk •") (gsvCode x0) "GSConstr"

gsbcviewpattern_w :: Pos -> GSValue -> [GSArg] -> GSExpr
gsbcviewpattern_w pos v ps = gsbcprof_w pos $ gsbcarg_w $gshere $ \ x -> gsbcapply_w $gshere v [
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

gsbcvarpattern_w :: Pos -> GSVar -> GSExpr
gsbcvarpattern_w pos v = gsbcprof_w pos $ gsbcarg_w pos $ \ x -> GSExpr $ \ msg pc cs sk ->
    gsreturn sk (GSRecord pos $ Map.fromList [(v, x)])

gsbcrunepattern_w :: Pos -> Char -> GSExpr
gsbcrunepattern_w pos ch = gsbcprof_w pos $ gsbcarg_w pos $ \ x -> gsbcforce_w pos ($gsav x) $ \ x0 -> case x0 of
    GSRune ch1 -> if ch == ch1
        then gsbcconstr_w pos (gsvar "1") [ $gsae $ gsbcrecord_w $gshere []]
        else gsbcconstr_w pos (gsvar "0") []
    _ -> gsbcimplementationfailure_w $gshere $ "gsbcrunepattern_w " ++ gsvCode x0 ++ " next"

gsbcdiscardpattern_w :: Pos -> GSExpr
gsbcdiscardpattern_w pos = gsbcprof_w pos $ gsbcarg_w pos $ \ x -> GSExpr $ \ msg pc cs sk ->
    gsreturn sk (GSRecord pos Map.empty)

gsbcprof_w :: Pos -> GSExpr -> GSExpr
gsbcprof_w pos e = GSExpr $ \ msg pc cs sk -> do
    prof pc msg (StackTrace pos cs)
    runGSExpr e msg pc cs sk

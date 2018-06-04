{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
module GSI.StdLib (gslambda, gscompose, gsapply_fn, gsanalyze, gsanalyzeImpM, gscase, gserror, gsundefined, gsfor, gsimpfor, gsimpunit) where

import Language.Haskell.TH.Lib (appE, varE)

import GSI.Syn (gsvar, fmtVarAtom)
import GSI.Value (GSValue(..), GSArg, GSExpr, GSExternal(..), gsundefined_value, gslambda_value, gsfield, gsav, gsae, gsvCode)
import API (apiImplementationFailure)
import GSI.Eval (evalSync)
import GSI.Functions (gsapiEvalString, gsapiEvalNatural)
import GSI.ByteCode (gsbcarg, gsbcapply, gsbcforce, gsbcerror, gsbcundefined, gsbcimpfor, gsbcimpbind, gsbcimpbody, gsbcimpunit, gsbcfmterrormsg, gsbcimplementationfailure)
import GSI.BCFunctions (gsbcevalstacktrace)

gslambda = $gslambda_value $ \ p -> $gsbcarg $ \ b -> $gsbcarg $ \ x ->
    $gsbcforce ($gsae $ $gsbcapply p [$gsav x]) $ \ r -> $gsbcapply b [$gsav r]

gscompose :: GSValue
gscompose = $gslambda_value $ \ f -> $gsbcarg $ \ g -> $gsbcarg $ \ x -> $gsbcapply f [$gsae $ $gsbcapply g [$gsav x]]

gsapply_fn = $gslambda_value $ \ f -> $gsbcarg $ \ x -> $gsbcapply f [ $gsav x ]

gsanalyze = $gslambda_value $ \ e -> $gsbcarg $ \ cs -> $gsbcapply cs [ $gsav e ]

gsanalyzeImpM = $gslambda_value $ \ a -> $gsbcarg $ \cs -> $gsbcimpfor $ do
    x <- $gsbcimpbind $ $gsav a
    $gsbcimpbody $ $gsae $ $gsbcapply cs [ $gsav x ]

gscase = $gslambda_value $ \ p -> $gsbcarg $ \ b -> $gsbcarg $ \ e -> $gsbcarg $ \ x ->
    $gsbcforce ($gsae $ $gsbcapply p [$gsav x]) $ \ c -> case c of
        GSConstr pos cv [r] | cv == gsvar "1" -> $gsbcapply b [$gsav r]
        GSConstr pos cv [] | cv == gsvar "0" -> $gsbcapply e [$gsav x]
        GSConstr pos cc args -> $gsbcimplementationfailure $ "gscase (pattern returns " ++ fmtVarAtom cc ") next" -- Probably §hs{$gsbcbranch ($gsav e) ($gsav b) c}
        _ -> $gsbcimplementationfailure $ "gscase (pattern returns " ++ gsvCode c ++ ") next" -- Probably §hs{$gsbcbranch ($gsav e) ($gsav b) c}

gsfor :: GSValue
gsfor = $gslambda_value $ \ g -> $gsbcarg $ \ e -> $gsbcforce ($gsav g) $ \ env -> $gsbcapply e [ $gsav env ]

gsimpfor :: GSValue
gsimpfor = $gslambda_value $ \ g -> $gsbcarg $ \ e -> $gsbcimpfor $ do
    env <- $gsbcimpbind $ $gsav g
    $gsbcimpbody $ $gsae $ $gsbcapply e [ $gsav env ]

gsimpunit = $gslambda_value $ \ x -> $gsbcimpfor $ do $gsbcimpunit $ $gsav x

gserror = $gslambda_value $ \ stv -> $gsbcarg $ \ msgv ->
    $gsbcevalstacktrace ($gsav stv) $ \ st_hs ->
    $gsbcfmterrormsg ($gsav msgv) $ \ msg_s ->
    gsbcerror st_hs msg_s

gsundefined = $gslambda_value $ \ stv -> $gsbcevalstacktrace ($gsav stv) $ \ st_hs ->
    gsbcundefined st_hs

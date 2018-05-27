{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
module GSI.StdLib (gslambda, gscompose, gsapply_fn, gsanalyze, gsanalyzeImpM, gscase, gserror, gsundefined, gsfor, gsimpfor, gsimpunit, gsbcevalpos, gsbcevalpos_w, gsapiEvalPos) where

import Language.Haskell.TH.Lib (appE, varE)

import GSI.Util (Pos(..), StackTrace(..), gshere)
import GSI.Syn (gsvar, fmtVarAtom)
import GSI.Value (GSValue(..), GSArg, GSExpr, GSExternal(..), gsundefined_value, gslambda_value, gsfield, gsav, gsae, gsvCode)
import API (apiImplementationFailure)
import GSI.Eval (evalSync)
import GSI.Functions (gsapiEvalString, gsapiEvalNatural)
import GSI.ByteCode (gsbcarg, gsbcapply, gsbcforce, gsbcfield, gsbcevalnatural, gsbcerror, gsbcundefined, gsbcimpfor, gsbcimpbind, gsbcimpbody, gsbcimpunit, gsbcfmterrormsg, gsbcimplementationfailure)
import GSI.BCFunctions (gsbcevalstring)

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
    gsbcevalstacktrace_w $gshere ($gsav stv) $ \ st_hs ->
    $gsbcfmterrormsg ($gsav msgv) $ \ msg_s ->
    gsbcerror st_hs msg_s

gsundefined = $gslambda_value $ \ stv -> gsbcevalstacktrace_w $gshere ($gsav stv) $ \ st_hs ->
    gsbcundefined st_hs

gsbcevalpos = varE 'gsbcevalpos_w `appE` gshere

gsbcevalpos_w :: Pos -> GSArg -> (Pos -> GSExpr) -> GSExpr
gsbcevalpos_w pos pos1a k = $gsbcforce pos1a $ \ pos1v -> case pos1v of
    GSRecord{} ->
        $gsbcevalstring ($gsae $ $gsbcfield ($gsav pos1v) (gsvar "filename")) $ \ pos_filename_s ->
        $gsbcevalnatural ($gsae $ $gsbcfield ($gsav pos1v) (gsvar "line")) $ \ pos_line_n ->
        $gsbcevalnatural ($gsae $ $gsbcfield ($gsav pos1v) (gsvar "col")) $ \ pos_col_n ->
        k $ Pos pos_filename_s pos_line_n pos_col_n
    GSExternal e -> case fromExternal e of
        Nothing -> $gsbcimplementationfailure $ "gsbcevalpos_w (GSExternal (not a Pos)) next"
        Just pos -> k pos
    _ -> $gsbcimplementationfailure $ "gsbcevalpos_w " ++ gsvCode pos1v ++ " next"

gsapiEvalPos :: Pos -> GSValue -> IO Pos
gsapiEvalPos pos (GSThunk th) = do
    v' <- evalSync [StackTrace pos []] th
    gsapiEvalPos pos v'
gsapiEvalPos pos v@GSRecord{} = do
    filename <- gsapiEvalString pos =<< $gsfield (gsvar "filename") v
    line <- gsapiEvalNatural pos =<< $gsfield (gsvar "line") v
    col <- gsapiEvalNatural pos =<< $gsfield (gsvar "col") v
    return $ Pos filename line col
gsapiEvalPos pos v = $apiImplementationFailure $ "gsapiEvalPos " ++ gsvCode v ++ " next"

gsbcevalstacktrace = varE 'gsbcevalstacktrace_w `appE` gshere

gsbcevalstacktrace_w :: Pos -> GSArg -> (StackTrace -> GSExpr) -> GSExpr
gsbcevalstacktrace_w pos pos1a k = $gsbcforce pos1a $ \ pos1v -> case pos1v of
    GSExternal e -> case fromExternal e of
        Nothing -> $gsbcimplementationfailure $ "gsbcevalstacktrace_w (GSExternal (not a Pos)) next"
        Just pos -> k pos
    _ -> $gsbcimplementationfailure $ "gsbcevalstacktrace_w " ++ gsvCode pos1v ++ " next"

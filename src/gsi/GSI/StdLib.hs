{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
module GSI.StdLib (gslambda, gscompose, gsapply_fn, gsanalyze, gsanalyzeImpM, gscase, gserror, gsundefined, gsfor, gsimpfor, gsimpunit, gsbcevalstring, gsbcevalstring_w, gsbcevalpos, gsbcevalpos_w) where

import Language.Haskell.TH.Lib (appE, varE)

import GSI.Util (Pos(..), StackTrace, gshere)
import GSI.Syn (gsvar, fmtVarAtom)
import GSI.Value (GSValue(..), GSArg, GSExpr, GSExternal(..), gsundefined_value, gslambda_value, gsav, gsae, gsvCode)
import GSI.ByteCode (gsbcundefined, gsbcarg, gsbcapply, gsbcforce, gsbcfield, gsbcevalnatural, gsbcerror, gsbcundefined_ww, gsbcimpfor, gsbcimpbind, gsbcimpbody, gsbcimpunit, gsbcfmterrormsg, gsbcimplementationfailure)

gslambda = $gslambda_value $ \ p -> $gsbcarg $ \ b -> $gsbcarg $ \ x ->
    $gsbcforce ($gsae $ $gsbcapply p [$gsav x]) $ \ c -> case c of
        GSConstr pos cv [r] | cv == gsvar "1" -> $gsbcapply b [$gsav r]
        GSConstr pos cc args -> $gsbcimplementationfailure $ "gslambda (pattern returns " ++ fmtVarAtom cc ") next"
        _ -> $gsbcimplementationfailure $ "gslambda (pattern returns " ++ gsvCode c ++ ") next"

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

-- This should be in GSI.String, but that would end up causing a circular dependency with this module so it goes here instead
gsbcevalstring = varE 'gsbcevalstring_w `appE` gshere

gsbcevalstring_w :: Pos -> GSArg -> (String -> GSExpr) -> GSExpr
gsbcevalstring_w pos sa k = w id sa where
    w :: (String -> String) -> GSArg -> GSExpr
    w ds0 sa = $gsbcforce sa $ \ sv -> case sv of
        GSConstr _ s_c [ c0, s1 ] | s_c == gsvar ":" ->
            $gsbcforce ($gsav c0) $ \ c0v -> case c0v of
                GSRune c0_hs -> w (ds0 . (c0_hs:)) ($gsav s1)
                _ -> $gsbcimplementationfailure $ "gsbcevalstring_w (GSConstr (:) " ++ gsvCode c0v ++ ") next"
        GSConstr _ s_c s_as | s_c == gsvar "nil" -> k (ds0 [])
        GSConstr _ s_c s_as -> $gsbcimplementationfailure $ "gsbcevalstring_w (GSConstr " ++ fmtVarAtom s_c ") next"
        _ -> $gsbcimplementationfailure $ "gsbcevalstring_w " ++ gsvCode sv ++ " next"

gserror = $gslambda_value $ \ stv -> $gsbcarg $ \ msgv ->
    gsbcevalstacktrace_w $gshere ($gsav stv) $ \ st_hs ->
    $gsbcfmterrormsg ($gsav msgv) $ \ msg_s ->
    gsbcerror st_hs msg_s

gsundefined = $gslambda_value $ \ stv -> gsbcevalstacktrace_w $gshere ($gsav stv) $ \ st_hs ->
    gsbcundefined_ww st_hs

gsbcevalpos = varE 'gsbcevalpos_w `appE` gshere

gsbcevalpos_w :: Pos -> GSArg -> (Pos -> GSExpr) -> GSExpr
gsbcevalpos_w pos pos1a k = $gsbcforce pos1a $ \ pos1v -> case pos1v of
    GSRecord{} ->
        gsbcevalstring_w $gshere ($gsae $ $gsbcfield ($gsav pos1v) (gsvar "filename")) $ \ pos_filename_s ->
        $gsbcevalnatural ($gsae $ $gsbcfield ($gsav pos1v) (gsvar "line")) $ \ pos_line_n ->
        $gsbcevalnatural ($gsae $ $gsbcfield ($gsav pos1v) (gsvar "col")) $ \ pos_col_n ->
        k $ Pos pos_filename_s pos_line_n pos_col_n
    GSExternal e -> case fromExternal e of
        Nothing -> $gsbcimplementationfailure $ "gsbcevalpos_w (GSExternal (not a Pos)) next"
        Just pos -> k pos
    _ -> $gsbcimplementationfailure $ "gsbcevalpos_w " ++ gsvCode pos1v ++ " next"

gsbcevalstacktrace = varE 'gsbcevalstacktrace_w `appE` gshere

gsbcevalstacktrace_w :: Pos -> GSArg -> (StackTrace -> GSExpr) -> GSExpr
gsbcevalstacktrace_w pos pos1a k = $gsbcforce pos1a $ \ pos1v -> case pos1v of
    GSExternal e -> case fromExternal e of
        Nothing -> $gsbcimplementationfailure $ "gsbcevalstacktrace_w (GSExternal (not a Pos)) next"
        Just pos -> k pos
    _ -> $gsbcimplementationfailure $ "gsbcevalstacktrace_w " ++ gsvCode pos1v ++ " next"

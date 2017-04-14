{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
module GSI.StdLib (gscompose, gsanalyze, gsanalyzeImpM, gscase, gserror, gsundefined, gsimpfor, gsbcevalstring, gsbcevalstring_w) where

import Language.Haskell.TH.Lib (appE, varE)

import GSI.Util (Pos(..), gshere)
import GSI.Syn (gsvar, fmtVarAtom)
import GSI.Value (GSValue(..), GSArg, GSExpr, gsundefined_value, gslambda, gsav, gsae, gsvCode)
import GSI.ByteCode (gsbcundefined, gsbcarg, gsbcapply, gsbcforce, gsbclfield, gsbcevalnatural, gsbcerror, gsbcundefined_w, gsbcimpfor, gsbcimpbind, gsbcimpbody, gsbcfmterrormsg, gsbcimplementationfailure)

gscompose :: GSValue
gscompose = $gslambda $ \ f -> $gsbcarg $ \ g -> $gsbcarg $ \ x -> $gsbcapply f [$gsae $ $gsbcapply g [$gsav x]]

gsanalyze = $gslambda $ \ e -> $gsbcarg $ \ cs -> $gsbcapply cs [ $gsav e ]

gsanalyzeImpM = $gslambda $ \ a -> $gsbcarg $ \cs -> $gsbcimpfor $ do
    x <- $gsbcimpbind $ $gsav a
    $gsbcimpbody $ $gsae $ $gsbcapply cs [ $gsav x ]

gscase = $gslambda $ \ p -> $gsbcarg $ \ b -> $gsbcarg $ \ e -> $gsbcarg $ \ x ->
    $gsbcforce ($gsae $ $gsbcapply p [$gsav x]) $ \ c -> case c of
        GSConstr pos cv [r] | cv == gsvar "1" -> $gsbcapply b [$gsav r]
        GSConstr pos cv [] | cv == gsvar "0" -> $gsbcapply e [$gsav x]
        GSConstr pos cc args -> $gsbcimplementationfailure $ "gscase (pattern returns " ++ fmtVarAtom cc ") next" -- Probably §hs{$gsbcbranch ($gsav e) ($gsav b) c}
        _ -> $gsbcimplementationfailure $ "gscase (pattern returns " ++ gsvCode c ++ ") next" -- Probably §hs{$gsbcbranch ($gsav e) ($gsav b) c}

gsimpfor :: GSValue
gsimpfor = $gslambda $ \ g -> $gsbcarg $ \ e -> $gsbcimpfor $ do
    env <- $gsbcimpbind $ $gsav g
    $gsbcimpbody $ $gsae $ $gsbcapply e [ $gsav env ]

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

gserror = $gslambda $ \ posv -> $gsbcarg $ \ msgv ->
    $gsbcforce ($gsav posv) $ \ posv0 -> case posv0 of
        GSRecord{} -> $gsbclfield (gsvar "filename") posv0 $ \ pos_filename ->
            gsbcevalstring_w $gshere ($gsav pos_filename) $ \ pos_filename_s ->
            $gsbclfield (gsvar "line") posv0 $ \ pos_line ->
            $gsbcevalnatural ($gsav pos_line) $ \ pos_line_n ->
            $gsbclfield (gsvar "col") posv0 $ \ pos_col ->
            $gsbcevalnatural ($gsav pos_col) $ \ pos_col_n ->
            let pos_hs = Pos pos_filename_s pos_line_n pos_col_n in
            $gsbcfmterrormsg ($gsav msgv) $ \ msg_s ->
                $gsbcerror pos_hs msg_s
        _ -> $gsbcimplementationfailure $ "gserror " ++ gsvCode posv0 ++ " next"
    -- > $gsbcerror pos msg

gsundefined = $gslambda $ \ posv -> $gsbcforce ($gsav posv) $ \ posv0 -> case posv0 of
    GSRecord{} -> $gsbclfield (gsvar "filename") posv0 $ \ pos_filename ->
        gsbcevalstring_w $gshere ($gsav pos_filename) $ \ pos_filename_s ->
        $gsbclfield (gsvar "line") posv0 $ \ pos_line ->
        $gsbcevalnatural ($gsav pos_line) $ \ pos_line_n ->
        $gsbclfield (gsvar "col") posv0 $ \ pos_col ->
        $gsbcevalnatural ($gsav pos_col) $ \ pos_col_n ->
        let pos_hs = Pos pos_filename_s pos_line_n pos_col_n in
        gsbcundefined_w pos_hs
    _ -> $gsbcimplementationfailure $ "gsundefined " ++ gsvCode posv0 ++ " next"

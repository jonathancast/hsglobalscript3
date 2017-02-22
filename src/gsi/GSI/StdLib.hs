{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
module GSI.StdLib (gscompose, gsanalyze, gscase, gserror) where

import GSI.Util (Pos(..))
import GSI.Syn (gsvar, fmtVarAtom)
import GSI.Value (GSValue(..), GSExpr, gsundefined, gslambda, gsav, gsae, gsvCode)
import GSI.ByteCode (gsbcundefined, gsbcarg, gsbcapply, gsbcforce, gsbcfield, gsbcevalstring, gsbcevalnatural, gsbcerror, gsbcfmterrormsg, gsbcimplementationfailure)

gscompose :: GSValue
gscompose = $gslambda $ \ f -> $gsbcarg $ \ g -> $gsbcarg $ \ x -> $gsbcapply f [$gsae $ $gsbcapply g [$gsav x]]

gsanalyze = $gslambda $ \ e -> $gsbcarg $ \ cs -> $gsbcapply cs [ $gsav e ]

gscase = $gslambda $ \ p -> $gsbcarg $ \ b -> $gsbcarg $ \ e -> $gsbcarg $ \ x ->
    $gsbcforce ($gsae $ $gsbcapply p [$gsav x]) $ \ c -> case c of
        GSConstr pos cv [r] | cv == gsvar "1" -> $gsbcapply b [$gsav r]
        GSConstr pos cc args -> $gsbcimplementationfailure $ "gscase (pattern returns " ++ fmtVarAtom cc ") next" -- Probably §hs{$gsbcbranch ($gsav e) ($gsav b) c}
        _ -> $gsbcimplementationfailure $ "gscase (pattern returns " ++ gsvCode c ++ ") next" -- Probably §hs{$gsbcbranch ($gsav e) ($gsav b) c}

gserror = $gslambda $ \ posv -> $gsbcarg $ \ msgv ->
    $gsbcforce ($gsav posv) $ \ posv0 -> case posv0 of
        GSRecord{} -> $gsbcfield (gsvar "filename") posv0 $ \ pos_filename ->
            $gsbcevalstring ($gsav pos_filename) $ \ pos_filename_s ->
            $gsbcfield (gsvar "line") posv0 $ \ pos_line ->
            $gsbcevalnatural ($gsav pos_line) $ \ pos_line_n ->
            $gsbcfield (gsvar "col") posv0 $ \ pos_col ->
            $gsbcevalnatural ($gsav pos_col) $ \ pos_col_n ->
            let pos_hs = Pos pos_filename_s pos_line_n pos_col_n in
            $gsbcfmterrormsg ($gsav msgv) $ \ msg_s ->
                $gsbcerror pos_hs msg_s
        _ -> $gsbcimplementationfailure $ "gserror " ++ gsvCode posv0 ++ " next"
    -- > $gsbcerror pos msg

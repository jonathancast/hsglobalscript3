{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
module GSI.StdLib (gsanalyze, gscase, gserror) where

import GSI.Syn (gsvar, fmtVarAtom)
import GSI.Value (GSValue(..), GSExpr, gsundefined, gslambda, gsav, gsargexpr, gsvCode)
import GSI.ByteCode (gsbcundefined, gsbcarg, gsbcapply, gsbcforce, gsbcfield, gsbcevalstring, gsbcerror, gsbcimplementationfailure)

gsanalyze = $gslambda $ \ e -> $gsbcarg $ \ cs -> $gsbcapply cs [ $gsav e ]

gscase = $gslambda $ \ p -> $gsbcarg $ \ b -> $gsbcarg $ \ e -> $gsbcarg $ \ x ->
    $gsbcforce ($gsargexpr $ $gsbcapply p [$gsav x]) $ \ c -> case c of
        GSConstr pos cv [r] | cv == gsvar "1" -> $gsbcapply b [$gsav r]
        GSConstr pos cc args -> $gsbcimplementationfailure $ "gscase (pattern returns " ++ fmtVarAtom cc ") next" -- Probably §hs{$gsbcbranch ($gsav e) ($gsav b) c}
        _ -> $gsbcimplementationfailure $ "gscase (pattern returns " ++ gsvCode c ++ ") next" -- Probably §hs{$gsbcbranch ($gsav e) ($gsav b) c}

gserror = $gslambda $ \ posv -> $gsbcarg $ \ msgv ->
    $gsbcforce ($gsav posv) $ \ posv0 -> case posv0 of
        GSRecord{} -> $gsbcfield (gsvar "filename") posv0 $ \ pos_filename ->
            $gsbcevalstring ($gsav pos_filename) $ \ pos_filename_s ->
                $gsbcimplementationfailure $ "gserror 〈 'filename = " ++ show pos_filename_s ++ "; 〉 next"
        _ -> $gsbcimplementationfailure $ "gserror " ++ gsvCode posv0 ++ " next"
    -- > $gsbcerror pos msg

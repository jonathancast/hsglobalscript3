{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
module GSI.StdLib (gsanalyze, gscase) where

import GSI.Syn (fmtVarAtom)
import GSI.Value (GSValue(..), gsundefined, gstoplevelclosure, gsvCode)
import GSI.ByteCode (gsbcundefined, gsbcapply, gsbcvar, gsbcforce, gsbcimplementationfailure)

gsanalyze = $gstoplevelclosure $ \ (e :: GSValue) (cs :: GSValue) -> $gsbcapply cs [ $gsbcvar e ]

gscase = $gstoplevelclosure $ \ (p :: GSValue) (b :: GSValue) (e :: GSValue) (x :: GSValue) ->
    $gsbcforce ($gsbcapply p [$gsbcvar x]) $ \ c -> case c of
        GSConstr pos cc args -> $gsbcimplementationfailure $ "gscase (pattern returns " ++ fmtVarAtom cc ") next" -- Probably §hs{$gsbcbranch ($gsbcvar e) ($gsbcvar b) c}
        _ -> $gsbcimplementationfailure $ "gscase (pattern returns " ++ gsvCode c ++ ") next" -- Probably §hs{$gsbcbranch ($gsbcvar e) ($gsbcvar b) c}

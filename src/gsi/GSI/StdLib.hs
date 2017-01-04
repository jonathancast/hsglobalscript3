{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
module GSI.StdLib (gsanalyze, gscase) where

import GSI.Syn (gsvar, fmtVarAtom)
import GSI.Value (GSValue(..), GSBCO, gsundefined, gslambda, gsvCode)
import GSI.ByteCode (gsbcundefined, gsbcapply, gsbcvar, gsbcforce, gsbcimplementationfailure)

gsanalyze = $gslambda $ \ (e :: GSValue) (cs :: GSValue) -> $gsbcapply cs [ $gsbcvar e ] :: GSBCO

gscase = $gslambda $ \ (p :: GSValue) (b :: GSValue) (e :: GSValue) (x :: GSValue) ->
    $gsbcforce ($gsbcapply p [$gsbcvar x]) $ \ c -> case c of
        GSConstr pos cv [r] | cv == gsvar "1" -> $gsbcapply b [$gsbcvar r]
        GSConstr pos cc args -> $gsbcimplementationfailure $ "gscase (pattern returns " ++ fmtVarAtom cc ") next" -- Probably §hs{$gsbcbranch ($gsbcvar e) ($gsbcvar b) c}
        _ -> $gsbcimplementationfailure $ "gscase (pattern returns " ++ gsvCode c ++ ") next" -- Probably §hs{$gsbcbranch ($gsbcvar e) ($gsbcvar b) c}

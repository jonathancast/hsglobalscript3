{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
module GSI.StdLib (gsanalyze, gscase, gserror) where

import GSI.Syn (gsvar, fmtVarAtom)
import GSI.Value (GSValue(..), GSExpr, gsundefined, gslambda, gsvCode)
import GSI.ByteCode (gsbcundefined, gsbclambda, gsbcapply, gsbcvar, gsbcforce, gsbcimplementationfailure)

gsanalyze = $gslambda $ \ e -> $gsbclambda $ \ cs -> $gsbcapply cs [ $gsbcvar e ] :: GSExpr

gscase = $gslambda $ \ p -> $gsbclambda $ \ b -> $gsbclambda $ \ e -> $gsbclambda $ \ x ->
    $gsbcforce ($gsbcapply p [$gsbcvar x]) $ \ c -> case c of
        GSConstr pos cv [r] | cv == gsvar "1" -> $gsbcapply b [$gsbcvar r]
        GSConstr pos cc args -> $gsbcimplementationfailure $ "gscase (pattern returns " ++ fmtVarAtom cc ") next" -- Probably §hs{$gsbcbranch ($gsbcvar e) ($gsbcvar b) c}
        _ -> $gsbcimplementationfailure $ "gscase (pattern returns " ++ gsvCode c ++ ") next" -- Probably §hs{$gsbcbranch ($gsbcvar e) ($gsbcvar b) c}

gserror = $gslambda $ \ (pos :: GSValue) -> $gsbcundefined

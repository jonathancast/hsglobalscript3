{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
module GSI.StdLib (gsanalyze, gscase, gserror) where

import GSI.Syn (gsvar, fmtVarAtom)
import GSI.Value (GSValue(..), GSExpr, gsundefined, gslambda, gsargvar, gsvCode)
import GSI.ByteCode (gsbcundefined, gsbcarg, gsbcapply, gsbcvar, gsbcforce, gsbcimplementationfailure)

gsanalyze = $gslambda $ \ e -> $gsbcarg $ \ cs -> $gsbcapply cs [ $gsargvar e ]

gscase = $gslambda $ \ p -> $gsbcarg $ \ b -> $gsbcarg $ \ e -> $gsbcarg $ \ x ->
    $gsbcforce ($gsbcapply p [$gsargvar x]) $ \ c -> case c of
        GSConstr pos cv [r] | cv == gsvar "1" -> $gsbcapply b [$gsargvar r]
        GSConstr pos cc args -> $gsbcimplementationfailure $ "gscase (pattern returns " ++ fmtVarAtom cc ") next" -- Probably §hs{$gsbcbranch ($gsargvar e) ($gsargvar b) c}
        _ -> $gsbcimplementationfailure $ "gscase (pattern returns " ++ gsvCode c ++ ") next" -- Probably §hs{$gsbcbranch ($gsargvar e) ($gsargvar b) c}

gserror = $gslambda $ \ (pos :: GSValue) -> $gsbcundefined

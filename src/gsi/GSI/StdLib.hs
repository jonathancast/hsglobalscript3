{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
module GSI.StdLib (gsanalyze, gscase) where

import GSI.Value (GSValue, gsundefined, gstoplevelclosure)
import GSI.ByteCode (gsbcundefined, gsbcapply, gsbcvar, gsbcforce)

gsanalyze = $gstoplevelclosure $ \ (e :: GSValue) (cs :: GSValue) -> $gsbcapply cs [ $gsbcvar e ]

gscase = $gstoplevelclosure $ \ (p :: GSValue) (b :: GSValue) (e :: GSValue) (x :: GSValue) ->
    $gsbcforce ($gsbcapply p [$gsbcundefined]) $ \ c -> -- Probably §hs{$gsbcvar x}
        $gsbcundefined -- Probably §hs{$gsbcbranch ($gsbcvar e) ($gsbcvar b) c}

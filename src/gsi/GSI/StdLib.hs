{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
module GSI.StdLib (gsanalyze, gscase) where

import GSI.Value (GSValue, gsundefined, gstoplevelclosure)
import GSI.ByteCode (gsbcundefined, gsbcapply, gsbcvar)

gsanalyze = $gstoplevelclosure $ \ (e :: GSValue) (cs :: GSValue) -> $gsbcapply cs [ $gsbcvar e ]

gscase = $gstoplevelclosure $ \ (p :: GSValue) (b :: GSValue) (e :: GSValue) (x :: GSValue) -> $gsbcundefined

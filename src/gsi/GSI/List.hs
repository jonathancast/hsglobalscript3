{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
module GSI.List (gscons_view) where

import GSI.Value (GSValue, gsundefined, gstoplevelclosure)
import GSI.ByteCode (gsbcundefined, gsbcvar, gsbcforce)

gscons_view = $gstoplevelclosure $ \ ek (sk :: GSValue) (x :: GSValue) ->
    $gsbcforce ($gsbcvar x) $ \ x0 ->
        $gsbcundefined

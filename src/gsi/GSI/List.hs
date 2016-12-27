{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
module GSI.List (gscons_view) where

import GSI.Value (GSValue, gsundefined, gstoplevelclosure, gsvCode)
import GSI.ByteCode (gsbcundefined, gsbcvar, gsbcforce, gsbcimplementationfailure)

gscons_view = $gstoplevelclosure $ \ ek (sk :: GSValue) (x :: GSValue) ->
    $gsbcforce ($gsbcvar x) $ \ x0 -> case x0 of
        _ -> $gsbcimplementationfailure $ "gscons_view " ++ gsvCode x0 ++ " next"

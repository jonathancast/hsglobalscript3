{-# LANGUAGE TemplateHaskell #-}
module GSI.Main (gsmain) where

import GSI.Value (GSBCO, gsundefined, gstoplevelclosure)
import GSI.ByteCode (gsbcundefined, gsbclambda, gsbcapply, gsbcprim, gsbcvar, gsbcimplet, gsbcimpbind, gsbcimpbody)
import GSI.CalculusPrims (gspriminsufficientcases)
import GSI.StdLib (gsanalyze)

-- Main function (call this to start your interpreter)
gsmain = $gstoplevelclosure $ \ gsrun -> do
    args <- $gsbcimpbind $ $gsbcundefined
    $gsbcimpbody $ $gsbcapply gsprocessargs [ $gsbcvar args ]

-- Loops over arguments to process them
gsprocessargs = $gstoplevelclosure $ \ args ->
    $gsbcapply gsanalyze [
        $gsbcvar args,
        $gsbclambda $ \ args -> $gsbcprim gspriminsufficientcases args :: GSBCO
    ]

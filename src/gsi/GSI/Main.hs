{-# LANGUAGE TemplateHaskell #-}
module GSI.Main (gsmain) where

import GSI.Value (gsundefined, gstoplevelclosure)
import GSI.ByteCode (gsbcundefined, gsbcapply, gsbcvar, gsbcimplet, gsbcimpbody)

-- Main function (call this to start your interpreter)
gsmain = $gstoplevelclosure $ \ gsrun -> do
    args <- $gsbcimplet $ $gsbcundefined
    $gsbcimpbody $ $gsbcapply gsprocessargs [ $gsbcvar args ]

-- Loops over arguments to process them
gsprocessargs = $gsundefined

{-# LANGUAGE TemplateHaskell #-}
module GSI.Main (gsmain) where

import GSI.Value (gstoplevelclosure)
import GSI.ByteCode (gsbcundefined, gsbcbody)

gsmain = $gstoplevelclosure $ \ gsrun -> do
    $gsbcbody $ $gsbcundefined


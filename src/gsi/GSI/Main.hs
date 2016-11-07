{-# LANGUAGE TemplateHaskell #-}
module GSI.Main (gsmain) where

import GSI.Value (gstoplevelclosure)
import GSI.ByteCode (gsbcundefined)

gsmain = $gstoplevelclosure $ \ gsrun -> $gsbcundefined

{-# LANGUAGE TemplateHaskell #-}
module GSI.List (gscons_view) where

import GSI.Value (gsundefined, gstoplevelclosure)
import GSI.ByteCode (gsbcundefined)

gscons_view = $gstoplevelclosure $ \ ek -> $gsbcundefined

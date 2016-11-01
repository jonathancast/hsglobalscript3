{-# LANGUAGE TemplateHaskell #-}
module GSI.Main (gsmain) where

import GSI.Value (gsundefined)
import GSI.ByteCode ()

gsmain = $gsundefined

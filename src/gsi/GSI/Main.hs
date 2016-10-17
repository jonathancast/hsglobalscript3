{-# LANGUAGE TemplateHaskell #-}
module GSI.Main (gsmain) where

import GSI.Value (gsundefined)

gsmain = $gsundefined

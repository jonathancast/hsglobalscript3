{-# LANGUAGE TemplateHaskell #-}
module GSI.GSI (gsigsundefined, gsicreateThread) where

import GSI.Value (gsundefined_value)

gsigsundefined = $gsundefined_value

gsicreateThread = $gsundefined_value

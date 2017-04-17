{-# LANGUAGE TemplateHaskell #-}
module GSI.GSI (gsigsundefined) where

import GSI.Value (gsundefined_value)

gsigsundefined = $gsundefined_value

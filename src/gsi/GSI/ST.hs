{-# LANGUAGE TemplateHaskell #-}
module GSI.ST (gsstrun) where

import GSI.Value (gsundefined_value)

gsstrun = $gsundefined_value

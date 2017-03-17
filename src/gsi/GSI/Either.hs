{-# LANGUAGE TemplateHaskell #-}
module GSI.Either (gsleft_view) where

import GSI.Value (GSValue, gslambda)
import GSI.ByteCode (gsbcarg, gsbcconstr_view)

gsleft_view :: GSValue
gsleft_view = $gslambda $ \ ek -> $gsbcarg $ \ sk -> $gsbcarg $ \ e -> $gsbcconstr_view "left" ek sk e

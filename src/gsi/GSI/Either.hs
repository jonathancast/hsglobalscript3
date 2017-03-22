{-# LANGUAGE TemplateHaskell #-}
module GSI.Either (gsleft_view, gsright_view) where

import GSI.Value (GSValue, gslambda)
import GSI.ByteCode (gsbcarg, gsbcconstr_view)

gsleft_view :: GSValue
gsleft_view = $gslambda $ \ ek -> $gsbcarg $ \ sk -> $gsbcarg $ \ e -> $gsbcconstr_view "left" ek sk e

gsright_view :: GSValue
gsright_view = $gslambda $ \ ek -> $gsbcarg $ \ sk -> $gsbcarg $ \ e -> $gsbcconstr_view "right" ek sk e

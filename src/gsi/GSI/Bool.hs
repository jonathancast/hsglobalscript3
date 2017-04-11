{-# LANGUAGE TemplateHaskell #-}
module GSI.Bool (gsfalse_view, gstrue_view) where

import GSI.Value (GSValue(..), gslambda)
import GSI.ByteCode (gsbcarg, gsbcconstr_view)

gsfalse_view :: GSValue
gsfalse_view = $gslambda $ \ ek -> $gsbcarg $ \ sk -> $gsbcarg $ \ b -> $gsbcconstr_view "false" ek sk b

gstrue_view :: GSValue
gstrue_view = $gslambda $ \ ek -> $gsbcarg $ \ sk -> $gsbcarg $ \ b -> $gsbcconstr_view "true" ek sk b

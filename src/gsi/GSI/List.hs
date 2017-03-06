{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
module GSI.List (gscons, gscons_view) where

import GSI.Value (GSValue, gsundefined, gslambda, gsvCode)
import GSI.ByteCode (gsbcundefined, gsbcarg, gsbcforce, gsbcimplementationfailure, gsbcconstr_view)

gscons :: GSValue
gscons = $gsundefined

gscons_view :: GSValue
gscons_view = $gslambda $ \ ek -> $gsbcarg $ \ sk -> $gsbcarg $ \ xn -> $gsbcconstr_view ":" ek sk xn

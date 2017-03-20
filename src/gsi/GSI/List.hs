{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
module GSI.List (gsappend, gscons, gscons_view) where

import GSI.Syn (gsvar)
import GSI.Value (GSValue, gsundefined, gslambda, gsav, gsvCode)
import GSI.ByteCode (gsbcundefined, gsbcarg, gsbcforce, gsbcimplementationfailure, gsbcconstr, gsbcconstr_view)

gsappend :: GSValue
gsappend = $gslambda $ \ xn -> $gsbcarg $ \ ys -> $gsbcundefined

gscons :: GSValue
gscons = $gslambda $ \ x -> $gsbcarg $ \ xn -> $gsbcconstr (gsvar ":") [ $gsav x, $gsav xn ]

gscons_view :: GSValue
gscons_view = $gslambda $ \ ek -> $gsbcarg $ \ sk -> $gsbcarg $ \ xn -> $gsbcconstr_view ":" ek sk xn

{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
module GSI.List (gscons_view) where

import GSI.Value (GSValue, gsundefined, gslambda, gsvCode)
import GSI.ByteCode (gsbcundefined, gsbclambda, gsbcvar, gsbcforce, gsbcimplementationfailure, gsbcconstr_view)

gscons_view = $gslambda $ \ ek -> $gsbclambda $ \ sk -> $gsbclambda $ \ xn -> $gsbcconstr_view ":" ek sk xn

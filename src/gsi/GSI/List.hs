{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
module GSI.List (gscons_view) where

import GSI.Value (GSValue, gsundefined, gstoplevelclosure, gsvCode)
import GSI.ByteCode (gsbcundefined, gsbcvar, gsbcforce, gsbcimplementationfailure, gsbcconstr_view)

gscons_view = $gstoplevelclosure $ $gsbcconstr_view ":"

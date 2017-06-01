{-# LANGUAGE TemplateHaskell #-}
module GSI.Bool (gsfalse, gsfalse_view, gstrue_view) where

import GSI.Util (gshere)
import GSI.Syn (gsvar)
import GSI.Value (GSValue(..), gslambda_value)
import GSI.ByteCode (gsbcarg, gsbcconstr_view)

gsfalse :: GSValue
gsfalse = GSConstr $gshere (gsvar "false") []

gsfalse_view :: GSValue
gsfalse_view = $gslambda_value $ \ ek -> $gsbcarg $ \ sk -> $gsbcarg $ \ b -> $gsbcconstr_view "false" ek sk b

gstrue_view :: GSValue
gstrue_view = $gslambda_value $ \ ek -> $gsbcarg $ \ sk -> $gsbcarg $ \ b -> $gsbcconstr_view "true" ek sk b

{-# LANGUAGE TemplateHaskell #-}
module GSI.Either (gsleft, gsleft_view, gsright_view) where

import GSI.Syn (gsvar)
import GSI.Value (GSValue, gslambda, gsav, gsundefined)
import GSI.ByteCode (gsbcarg, gsbcconstr, gsbcconstr_view, gsbcundefined)

gsleft :: GSValue
gsleft = $gslambda $ \ x -> $gsbcconstr (gsvar "left") [ $gsav x ]

gsleft_view :: GSValue
gsleft_view = $gslambda $ \ ek -> $gsbcarg $ \ sk -> $gsbcarg $ \ e -> $gsbcconstr_view "left" ek sk e

gsright_view :: GSValue
gsright_view = $gslambda $ \ ek -> $gsbcarg $ \ sk -> $gsbcarg $ \ e -> $gsbcconstr_view "right" ek sk e

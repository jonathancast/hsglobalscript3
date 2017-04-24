{-# LANGUAGE TemplateHaskell #-}
module GSI.Either (gseitherFor, gseitherbind, gseithermap, gsleft, gsright, gsleft_view, gsright_view) where

import GSI.Syn (gsvar)
import GSI.Value (GSValue, gslambda, gsav, gsundefined_value)
import GSI.ByteCode (gsbcarg, gsbcprim, gsbcconstr, gsbcconstr_view, gsbcundefined)
import GSI.CalculusPrims (gspriminsufficientcases)

gseitherFor :: GSValue
gseitherFor = $gslambda $ \ gen -> $gsbcarg $ \ k -> $gsbcprim gspriminsufficientcases gen

gseitherbind :: GSValue
gseitherbind = $gsundefined_value

gseithermap :: GSValue
gseithermap = $gsundefined_value

gsleft :: GSValue
gsleft = $gslambda $ \ x -> $gsbcconstr (gsvar "left") [ $gsav x ]

gsright :: GSValue
gsright = $gslambda $ \ x -> $gsbcconstr (gsvar "right") [ $gsav x ]

gsleft_view :: GSValue
gsleft_view = $gslambda $ \ ek -> $gsbcarg $ \ sk -> $gsbcarg $ \ e -> $gsbcconstr_view "left" ek sk e

gsright_view :: GSValue
gsright_view = $gslambda $ \ ek -> $gsbcarg $ \ sk -> $gsbcarg $ \ e -> $gsbcconstr_view "right" ek sk e

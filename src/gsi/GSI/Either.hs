{-# LANGUAGE TemplateHaskell #-}
module GSI.Either (gseitherFor, gseitherbind, gseithermap, gsleft, gsright, gsleft_view, gsright_view) where

import GSI.Syn (gsvar)
import GSI.Value (GSValue, gslambda, gsav, gsae, gsundefined_value)
import GSI.ByteCode (gsbcarg, gsbclfield, gsbcapply, gsbcprim, gsbcconstr, gsbcviewpattern, gsbcconstr_view, gsbcvarpattern, gsbcundefined)
import GSI.CalculusPrims (gspriminsufficientcases)
import GSI.StdLib (gsanalyze, gscase)

gseitherFor :: GSValue
gseitherFor = $gslambda $ \ gen -> $gsbcarg $ \ k -> $gsbcapply gseitherbind [ $gsav gen, $gsav k ]

gseitherbind :: GSValue
gseitherbind = $gslambda $ \ e -> $gsbcarg $ \ k -> $gsbcapply gsanalyze [ $gsav e,
    $gsae $ $gsbcapply gscase [ $gsae $ $gsbcviewpattern gsright_view ($gsbcvarpattern "x"),
        $gsae $ $gsbcarg $ \ env -> $gsbclfield (gsvar "x") env $ \ x -> $gsbcapply k [ $gsav x ],
    $gsae $ $gsbcarg $ \ e -> $gsbcprim gspriminsufficientcases e
  ]]

gseithermap :: GSValue
gseithermap = $gslambda $ \ f -> $gsbcarg $ \ e -> $gsbcapply gsanalyze [ $gsav e,
    $gsae $ $gsbcapply gscase [ $gsae $ $gsbcviewpattern gsright_view ($gsbcvarpattern "x"),
        $gsae $ $gsbcarg $ \ env -> $gsbclfield (gsvar "x") env $ \ x -> $gsbcapply gsright [ $gsae $ $gsbcundefined ],
    $gsae $ $gsbcarg $ \ e -> $gsbcprim gspriminsufficientcases e
  ]]

gsleft :: GSValue
gsleft = $gslambda $ \ x -> $gsbcconstr (gsvar "left") [ $gsav x ]

gsright :: GSValue
gsright = $gslambda $ \ x -> $gsbcconstr (gsvar "right") [ $gsav x ]

gsleft_view :: GSValue
gsleft_view = $gslambda $ \ ek -> $gsbcarg $ \ sk -> $gsbcarg $ \ e -> $gsbcconstr_view "left" ek sk e

gsright_view :: GSValue
gsright_view = $gslambda $ \ ek -> $gsbcarg $ \ sk -> $gsbcarg $ \ e -> $gsbcconstr_view "right" ek sk e

{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
module GSI.List (gsappend, gscons, gscons_view) where

import GSI.Syn (gsvar)
import GSI.Value (GSValue, gsundefined, gslambda, gsae, gsav, gsvCode)
import GSI.ByteCode (gsbcundefined, gsbcarg, gsbcapply, gsbcprim, gsbcenter, gsbcfield, gsbcforce, gsbcimplementationfailure, gsbcconstr, gsbcconstr_view, gsbcviewpattern, gsbcvarpattern)
import GSI.CalculusPrims (gspriminsufficientcases)
import GSI.StdLib (gsanalyze, gscase)

gsappend :: GSValue
gsappend = $gslambda $ \ xn -> $gsbcarg $ \ ys -> $gsbcapply gsanalyze [ $gsav xn,
    $gsae $ $gsbcapply gscase [ $gsae $ $gsbcviewpattern gscons_view ($gsbcvarpattern "x") ($gsbcvarpattern "xn1"),
        $gsae $ $gsbcarg $ \ env -> $gsbcfield (gsvar "x") env $ \ x -> $gsbcfield (gsvar "xn1") env $ \ xn1 ->
            $gsbcapply gscons [ $gsav x, $gsae $ $gsbcapply gsappend [ $gsav xn1, $gsav ys ] ]
    ,
    $gsae $ $gsbcapply gscase [ $gsae $ $gsbcviewpattern gsnil_view,
        $gsae $ $gsbcarg $ \ env -> $gsbcenter ys
    ,
    $gsae $ $gsbcarg $ \ args -> $gsbcprim gspriminsufficientcases args
  ]]]

gscons :: GSValue
gscons = $gslambda $ \ x -> $gsbcarg $ \ xn -> $gsbcconstr (gsvar ":") [ $gsav x, $gsav xn ]

gscons_view :: GSValue
gscons_view = $gslambda $ \ ek -> $gsbcarg $ \ sk -> $gsbcarg $ \ xn -> $gsbcconstr_view ":" ek sk xn

gsnil_view :: GSValue
gsnil_view = $gslambda $ \ ek -> $gsbcarg $ \ sk -> $gsbcarg $ \ xn -> $gsbcconstr_view "nil" ek sk xn

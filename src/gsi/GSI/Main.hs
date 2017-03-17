{-# LANGUAGE TemplateHaskell #-}
module GSI.Main (gsmain) where

import GSI.Syn (gsvar)
import GSI.Value (GSExpr, gsundefined, gslambda, gsav, gsae)
import GSI.ByteCode (gsbcundefined, gsbcarg, gsbcapply, gsbcprim, gsbcfield, gsbcviewpattern, gsbcvarpattern, gsbchere, gsbcimpfor, gsbcimplet, gsbcimpbind, gsbcimpbody)
import GSI.CalculusPrims (gspriminsufficientcases)
import GSI.StdLib (gserror, gsanalyze, gscase)
import GSI.List (gscons_view)
import GSI.Log (gsbclog, gsbclogstring, gsloggsv)
import GSI.Env (gsenvGetArgs)

-- Main function (call this to start your interpreter)
gsmain = $gslambda $ \ gsrun -> $gsbcimpfor $ do
    args <- $gsbcimpbind $ $gsav gsenvGetArgs
    $gsbcimpbody $ $gsae $ $gsbcapply gsprocessargs [ $gsav args ]

-- Loops over arguments to process them
gsprocessargs = $gslambda $ \ args ->
    $gsbcapply gsanalyze [ $gsav args,
        $gsae ($gsbcapply gscase [ $gsae $ $gsbcviewpattern gscons_view ($gsbcvarpattern "a") ($gsbcvarpattern "as"),
            $gsae $ $gsbcarg $ \ env -> $gsbcfield (gsvar "a") env $ \ a -> $gsbcimpfor $ do
                $gsbcimpbody $ $gsae $
                    $gsbcapply gserror [ $gsae $gsbchere, $gsae $ $gsbclog [ $gsae $ $gsbclogstring "Process ", $gsae $ $gsbcapply gsloggsv [ $gsav a ], $gsae $ $gsbclogstring " next" ] ],
        $gsae $ $gsbcarg $ \ args -> $gsbcprim gspriminsufficientcases args
        ])
    ]

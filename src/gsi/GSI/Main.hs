{-# LANGUAGE TemplateHaskell #-}
module GSI.Main (gsmain) where

import GSI.Value (GSBCO, gsundefined, gslambda)
import GSI.ByteCode (gsbcundefined, gsbclambda, gsbcapply, gsbcprim, gsbcvar, gsbcviewpattern, gsbcvarpattern, gsbcimpfor, gsbcimplet, gsbcimpbind, gsbcimpbody)
import GSI.CalculusPrims (gspriminsufficientcases)
import GSI.StdLib (gsanalyze, gscase)
import GSI.List (gscons_view)
import GSI.Env (gsenvGetArgs)

-- Main function (call this to start your interpreter)
gsmain = $gslambda $ \ gsrun -> $gsbcimpfor $ do
    args <- $gsbcimpbind $ $gsbcvar gsenvGetArgs
    $gsbcimpbody $ $gsbcapply gsprocessargs [ $gsbcvar args ]

-- Loops over arguments to process them
gsprocessargs = $gslambda $ \ args ->
    $gsbcapply gsanalyze [ $gsbcvar args,
        $gsbcapply gscase [ $gsbcviewpattern ($gsbcvar gscons_view) ($gsbcvarpattern "a") ($gsbcvarpattern "as"),
            $gsbclambda $ \ env -> $gsbcundefined, -- Process §gs{a:as}
        $gsbclambda $ \ args -> $gsbcprim gspriminsufficientcases args :: GSBCO
        ]
    ]

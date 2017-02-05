{-# LANGUAGE TemplateHaskell #-}
module GSI.Main (gsmain) where

import GSI.Value (GSExpr, gsundefined, gslambda)
import GSI.ByteCode (gsbcundefined, gsbcarg, gsbcapply, gsbcprim, gsbcvar, gsbcviewpattern, gsbcvarpattern, gsbchere, gsbcimpfor, gsbcimplet, gsbcimpbind, gsbcimpbody)
import GSI.CalculusPrims (gspriminsufficientcases)
import GSI.StdLib (gserror, gsanalyze, gscase)
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
            $gsbcarg $ \ env -> $gsbcapply gserror [ $gsbchere, $gsbcundefined ], -- Process §gs{a:as}
        $gsbcarg $ \ args -> $gsbcprim gspriminsufficientcases args :: GSExpr
        ]
    ]

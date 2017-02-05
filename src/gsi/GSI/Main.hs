{-# LANGUAGE TemplateHaskell #-}
module GSI.Main (gsmain) where

import GSI.Value (GSExpr, gsundefined, gslambda, gsargvar, gsargexpr)
import GSI.ByteCode (gsbcundefined, gsbcarg, gsbcapply, gsbcprim, gsbcvar, gsbcviewpattern, gsbcvarpattern, gsbchere, gsbcimpfor, gsbcimplet, gsbcimpbind, gsbcimpbody)
import GSI.CalculusPrims (gspriminsufficientcases)
import GSI.StdLib (gserror, gsanalyze, gscase)
import GSI.List (gscons_view)
import GSI.Env (gsenvGetArgs)

-- Main function (call this to start your interpreter)
gsmain = $gslambda $ \ gsrun -> $gsbcimpfor $ do
    args <- $gsbcimpbind $ $gsbcvar gsenvGetArgs
    $gsbcimpbody $ $gsbcapply gsprocessargs [ $gsargvar args ]

-- Loops over arguments to process them
gsprocessargs = $gslambda $ \ args ->
    $gsbcapply gsanalyze [ $gsargvar args,
        $gsargexpr ($gsbcapply gscase [ $gsargexpr $ $gsbcviewpattern ($gsbcvar gscons_view) ($gsbcvarpattern "a") ($gsbcvarpattern "as"),
            $gsargexpr $ $gsbcarg $ \ env -> $gsbcapply gserror [ $gsargexpr $gsbchere, $gsargexpr $gsbcundefined ], -- Process §gs{a:as}
        $gsargexpr $ $gsbcarg $ \ args -> $gsbcprim gspriminsufficientcases args
        ])
    ]

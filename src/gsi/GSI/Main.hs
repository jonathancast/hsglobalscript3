{-# LANGUAGE TemplateHaskell #-}
module GSI.Main (gsmain) where

import GSI.Syn (gsvar)
import GSI.Value (GSExpr, gsundefined, gslambda, gsargvar, gsae)
import GSI.ByteCode (gsbcundefined, gsbcarg, gsbcapply, gsbcprim, gsbcvar, gsbcviewpattern, gsbcvarpattern, gsbchere, gsbcimpfor, gsbcimplet, gsbcimpbind, gsbcimpbody)
import GSI.CalculusPrims (gspriminsufficientcases)
import GSI.StdLib (gserror, gsanalyze, gscase)
import GSI.List (gscons_view)
import GSI.Log (gsbclog, gsbclogstring, gsloggs)
import GSI.Env (gsenvGetArgs)

-- Main function (call this to start your interpreter)
gsmain = $gslambda $ \ gsrun -> $gsbcimpfor $ do
    args <- $gsbcimpbind $ $gsargvar gsenvGetArgs
    $gsbcimpbody $ $gsae $ $gsbcapply gsprocessargs [ $gsargvar args ]

-- Loops over arguments to process them
gsprocessargs = $gslambda $ \ args ->
    $gsbcapply gsanalyze [ $gsargvar args,
        $gsae ($gsbcapply gscase [ $gsae $ $gsbcviewpattern gscons_view ($gsbcvarpattern "a") ($gsbcvarpattern "as"),
            $gsae $ $gsbcarg $ \ env ->
                $gsbcapply gserror [ $gsae $gsbchere, $gsae $ $gsbclog [ $gsae $ $gsbclogstring "Process ", $gsae $ $gsbcapply gsloggs [ $gsae $ $gsbcvar (gsvar "a") env ], $gsae $gsbcundefined ] ], -- > log{Process }, gs gs{a}, log{ next}
                -- Process §gs{a:as}
        $gsae $ $gsbcarg $ \ args -> $gsbcprim gspriminsufficientcases args
        ])
    ]

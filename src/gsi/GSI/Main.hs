{-# LANGUAGE TemplateHaskell #-}
module GSI.Main (gsmain) where

import GSI.Syn (gsvar)
import GSI.Value (GSExpr, gsundefined, gslambda, gsav, gsae)
import GSI.ByteCode (gsbcundefined, gsbcarg, gsbcapply, gsbcprim, gsbcfield, gsbcviewpattern, gsbcvarpattern, gsbchere, gsbcimpfor, gsbcimplet, gsbcimpbind, gsbcimpbody)
import GSI.CalculusPrims (gspriminsufficientcases)
import GSI.StdLib (gserror, gsanalyze, gscase)
import GSI.List (gscons_view)
import GSI.String (gsbcstring, gsbcstringlit)
import GSI.Either (gsleft_view)
import GSI.Log (gsbclog, gsbclogstring, gsloggsv)
import GSI.Env (gsenvGetArgs, gsfileStat, gsprintError, gsENOENT_view)

-- Main function (call this to start your interpreter)
gsmain = $gslambda $ \ gsrun -> $gsbcimpfor $ do
    args <- $gsbcimpbind $ $gsav gsenvGetArgs
    $gsbcimpbody $ $gsae $ $gsbcapply gsprocessargs [ $gsav args ]

-- Loops over arguments to process them
gsprocessargs = $gslambda $ \ args ->
    $gsbcapply gsanalyze [ $gsav args,
        $gsae ($gsbcapply gscase [ $gsae $ $gsbcviewpattern gscons_view ($gsbcvarpattern "a") ($gsbcvarpattern "as"),
            $gsae $ $gsbcarg $ \ env -> $gsbcfield (gsvar "a") env $ \ a -> $gsbcimpfor $ do
                st <- $gsbcimpbind $ $gsae $ $gsbcapply gsfileStat [ $gsav a ]
                $gsbcimpbody $ $gsae $
                    $gsbcapply gsanalyze [ $gsav st,
                        $gsae $ $gsbcapply gscase [ $gsae $ $gsbcviewpattern gsleft_view ($gsbcvarpattern "e"),
                            $gsae $ $gsbcarg $ \ env -> $gsbcfield (gsvar "e") env $ \ e ->
                                $gsbcapply gsanalyze [ $gsav e,
                                    $gsae $ $gsbcapply gscase [ $gsae $ $gsbcviewpattern gsENOENT_view ($gsbcvarpattern "fn"),
                                        $gsae $ $gsbcarg $ \ env -> $gsbcimpfor $ do
                                            $gsbcimpbind $ $gsae $ $gsbcapply gsprintError [ $gsae $ $gsbcstring [ $gsav $ a, $gsae $ $gsbcstringlit "\n" ] ]
                                            $gsbcimpbody $ $gsae $ $gsbcapply gserror [ $gsae $gsbchere, $gsae $ $gsbclog [ $gsae $ $gsbclogstring "Process ", $gsae $ $gsbcapply gsloggsv [ $gsav a ], $gsae $ $gsbclogstring " (", $gsae $ $gsbcapply gsloggsv [ $gsav e ], $gsae $ $gsbclogstring ") next" ] ],
                                    $gsae $ $gsbcapply gserror [ $gsae $gsbchere, $gsae $ $gsbclog [ $gsae $ $gsbclogstring "Process ", $gsae $ $gsbcapply gsloggsv [ $gsav a ], $gsae $ $gsbclogstring " (", $gsae $ $gsbcapply gsloggsv [ $gsav e ], $gsae $ $gsbclogstring ") next" ] ]
                                ] ]
                        ,
                        $gsae $ $gsbcapply gserror [ $gsae $gsbchere, $gsae $ $gsbclog [ $gsae $ $gsbclogstring "Process ", $gsae $ $gsbcapply gsloggsv [ $gsav a ], $gsae $ $gsbclogstring " (", $gsae $ $gsbcapply gsloggsv [ $gsav st ], $gsae $ $gsbclogstring ") next" ] ]
                    ] ]
        ,
        $gsae $ $gsbcarg $ \ args -> $gsbcprim gspriminsufficientcases args
        ])
    ]

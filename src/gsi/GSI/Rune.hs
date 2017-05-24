{-# LANGUAGE TemplateHaskell #-}
module GSI.Rune (gsisLower, gsrune_neq, gsruneEq) where

import GSI.Syn (gsvar)
import GSI.Value (GSValue(..), gslambda_value, gsundefined_value, gsav, gsvCode)
import GSI.ByteCode (gsbcarg, gsbcforce, gsbcconstr, gsbcundefined, gsbcimplementationfailure)

gsisLower = $gsundefined_value

gsrune_neq = $gslambda_value $ \ c0 -> $gsbcarg $ \ c1 -> $gsbcforce ($gsav c0) $ \ c0_0 -> $gsbcforce ($gsav c1) $ \ c1_0 -> case (c0_0, c1_0) of
    (GSRune c0hs, GSRune c1hs) ->
        if c0hs /= c1hs then $gsbcconstr (gsvar "true") [] else $gsbcconstr (gsvar "false") []
    _ -> $gsbcimplementationfailure $ "gsrune_neq " ++ gsvCode c0_0 ++ ' ' : gsvCode c1_0 ++ " next"

gsruneEq = $gslambda_value $ \ r0 -> $gsbcarg $ \ r1 -> $gsbcforce ($gsav r0) $ \ r0v -> $gsbcforce ($gsav r1) $ \ r1v ->
    case (r0v, r1v) of
        (GSRune c0, GSRune c1)
            | c0 == c1 -> $gsbcconstr (gsvar "true") []
            | otherwise -> $gsbcconstr (gsvar "false") []
        _ -> $gsbcimplementationfailure $ "gsruneEq " ++ gsvCode r0v ++ ' ' : gsvCode r1v ++ " next"

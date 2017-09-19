{-# LANGUAGE TemplateHaskell #-}
module GSI.Rune (gsisLower, gsisSpace, gsrune_neq, gsruneEq) where

import Data.Char (isLower, isSpace)

import GSI.Syn (gsvar)
import GSI.Value (GSValue(..), gslambda_value, gsundefined_value, gsav, gsvCode)
import GSI.ByteCode (gsbcarg, gsbcforce, gsbcconstr, gsbcimplementationfailure)

gsisLower = $gslambda_value $ \ r -> $gsbcforce ($gsav r) $ \ r0 -> case r0 of
    GSRune r0v -> if isLower r0v then $gsbcconstr (gsvar "true") [] else $gsbcconstr (gsvar "false") []
    _ -> $gsbcimplementationfailure $ "gsisLower " ++ gsvCode r0 ++ " next"

gsisSpace = $gslambda_value $ \ r -> $gsbcforce ($gsav r) $ \ r0 -> case r0 of
    GSRune r0v -> if isSpace r0v then $gsbcconstr (gsvar "true") [] else $gsbcconstr (gsvar "false") []
    _ -> $gsbcimplementationfailure $ "gsisSpace " ++ gsvCode r0 ++ " next"

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

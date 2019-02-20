{-# LANGUAGE TemplateHaskell #-}
module GSI.Rune (gsisAsciiDigit, gsisLower, gsis_upper, gsisSpace, gsrune_code_point, gsrune_from_code_point, gsrune_compare, gsrune_neq, gsruneEq) where

import Data.Char (isDigit, isLower, isUpper, isSpace, chr, ord)

import GSI.Syn (gsvar)
import GSI.Value (GSValue(..), gslambda_value, gsundefined_value, gsav, gsvCode)
import GSI.ByteCode (gsbcarg, gsbcforce, gsbcconstr, gsbcnatural, gsbcrune, gsbcimplementationfailure)

gsisAsciiDigit = hspred2gspred isDigit

gsisLower = hspred2gspred isLower
gsis_upper = hspred2gspred isUpper

gsisSpace = hspred2gspred isSpace

hspred2gspred p = $gslambda_value $ \ r -> $gsbcforce ($gsav r) $ \ r0 -> case r0 of
    GSRune r0v -> if p r0v then $gsbcconstr (gsvar "true") [] else $gsbcconstr (gsvar "false") []
    _ -> $gsbcimplementationfailure $ "hspred2gspred " ++ gsvCode r0 ++ " next"

gsrune_code_point = $gslambda_value $ \ r -> $gsbcforce ($gsav r) $ \ r0 -> case r0 of
    GSRune r0v -> $gsbcnatural $ toInteger $ ord r0v
    _ -> $gsbcimplementationfailure $ "gsrune_code_point " ++ gsvCode r0 ++ " next"

gsrune_from_code_point = $gslambda_value $ \ n -> $gsbcforce ($gsav n) $ \ n0 -> case n0 of
    GSNatural n0hs -> $gsbcrune $ chr $ fromInteger n0hs
    _ -> $gsbcimplementationfailure $ "gsrune_from_code_point " ++ gsvCode n0 ++ " next"

gsrune_compare = $gslambda_value $ \ c0 -> $gsbcarg $ \ c1 -> $gsbcforce ($gsav c0) $ \ c0_0 -> $gsbcforce ($gsav c1) $ \ c1_0 -> case (c0_0, c1_0) of
    (GSRune c0hs, GSRune c1hs) -> case c0hs `compare` c1hs of
        LT -> $gsbcconstr (gsvar "lt") []
        EQ -> $gsbcconstr (gsvar "eq") []
        GT -> $gsbcconstr (gsvar "gt") []
    _ -> $gsbcimplementationfailure $ "gsrune_neq " ++ gsvCode c0_0 ++ ' ' : gsvCode c1_0 ++ " next"

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

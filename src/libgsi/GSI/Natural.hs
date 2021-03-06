{-# LANGUAGE TemplateHaskell #-}
module GSI.Natural (gsnatural_unary_plus, gsnatural_add, gsnatural_addition, gsnatural_subtract, gsnatural_subtract_maybe, gsnatural_multiply, gsnatural_div_mod, gsnatural_max, gsnatural_eq, gsnatural_neq, gsnatural_lt, gsnatural_le, gsnatural_gt, gsnatural_cmp) where

import qualified Data.Map as Map

import GSI.Util (gshere)
import GSI.Syn (gsvar)
import GSI.Value (GSValue(..), gslambda_value, gsav, gsvCode)
import GSI.ByteCode (gsbcarg, gsbcforce, gsbcenter, gsbcnatural, gsbcconstr, gsbcrecord, gsbcimplementationfailure)

gsnatural_unary_plus = $gslambda_value $ \ n -> $gsbcenter n

gsnatural_add = gsnatural_op (+)

gsnatural_addition = GSRecord $gshere $ Map.fromList [
    (gsvar "0", GSNatural [] 0),
    (gsvar "+", gsnatural_add)
  ]

gsnatural_subtract = $gslambda_value $ \ n0 -> $gsbcarg $ \ n1 -> $gsbcforce ($gsav n0) $ \ n0_0 -> $gsbcforce ($gsav n1) $ \ n1_0 -> case (n0_0, n1_0) of
    (GSNatural _ n0hs, GSNatural _ n1hs) -> let dhs = n0hs - n1hs in case dhs >= 0 of
        True -> $gsbcnatural dhs
        False -> $gsbcimplementationfailure $ "gsnatural_subtract: result is " ++ show dhs ++ " next"
    _ -> $gsbcimplementationfailure $ "gsnatural_subtract " ++ gsvCode n0_0 ++ ' ' : gsvCode n1_0 ++ " next"

gsnatural_subtract_maybe = $gslambda_value $ \ n0 -> $gsbcarg $ \ n1 -> $gsbcforce ($gsav n0) $ \ n0_0 -> $gsbcforce ($gsav n1) $ \ n1_0 -> case (n0_0, n1_0) of
    (GSNatural _ n0hs, GSNatural _ n1hs) -> let dhs = n0hs - n1hs in case dhs >= 0 of
        True -> $gsbcconstr (gsvar "just") [ $gsav $ GSNatural [] dhs ]
        False -> $gsbcconstr (gsvar "nothing") []
    _ -> $gsbcimplementationfailure $ "gsnatural_subtract_maybe " ++ gsvCode n0_0 ++ ' ' : gsvCode n1_0 ++ " next"

gsnatural_multiply = gsnatural_op (*)

gsnatural_div_mod = $gslambda_value $ \ n0 -> $gsbcarg $ \ n1 -> $gsbcforce ($gsav n0) $ \ n0_0 -> $gsbcforce ($gsav n1) $ \ n1_0 -> case (n0_0, n1_0) of
    (GSNatural _ n0hs, GSNatural _ n1hs) -> case n1hs == 0 of
        False -> $gsbcrecord [ (gsvar "0", $gsav $ GSNatural [] $ n0hs `div` n1hs), (gsvar "1", $gsav $ GSNatural [] $ n0hs `mod` n1hs) ]
        True -> $gsbcimplementationfailure $ "gsnatural_div_mod n0hs 0 next"
    _ -> $gsbcimplementationfailure $ "gsnatural_div_mod " ++ gsvCode n0_0 ++ ' ' : gsvCode n1_0 ++ " next"

gsnatural_op f = $gslambda_value $ \ n0 -> $gsbcarg $ \ n1 -> $gsbcforce ($gsav n0) $ \ n0_0 -> $gsbcforce ($gsav n1) $ \ n1_0 -> case (n0_0, n1_0) of
    (GSNatural _ n0hs, GSNatural _ n1hs) -> $gsbcenter $ GSNatural [] $ f n0hs n1hs
    _ -> $gsbcimplementationfailure $ "gsnatural_op " ++ gsvCode n0_0 ++ ' ' : gsvCode n1_0 ++ " next"

gsnatural_max = gsnatural_op max

gsnatural_eq = gsnatural_rel (==)

gsnatural_neq = gsnatural_rel (/=)

gsnatural_lt = gsnatural_rel (<)

gsnatural_le = gsnatural_rel (<=)

gsnatural_gt = gsnatural_rel (>)

gsnatural_rel rel = $gslambda_value $ \ n0 -> $gsbcarg $ \ n1 -> $gsbcforce ($gsav n0) $ \ n0_0 -> $gsbcforce ($gsav n1) $ \ n1_0 -> case (n0_0, n1_0) of
    (GSNatural _ n0hs, GSNatural _ n1hs) ->
        if rel n0hs n1hs then $gsbcconstr (gsvar "true") [] else $gsbcconstr (gsvar "false") []
    _ -> $gsbcimplementationfailure $ "gsnatural_rel " ++ gsvCode n0_0 ++ ' ' : gsvCode n1_0 ++ " next"

gsnatural_cmp = $gslambda_value $ \ n0 -> $gsbcarg $ \ n1 -> $gsbcforce ($gsav n0) $ \ n0_0 -> $gsbcforce ($gsav n1) $ \ n1_0 -> case (n0_0, n1_0) of
    (GSNatural _ n0hs, GSNatural _ n1hs) -> case n0hs `compare` n1hs of
        LT -> $gsbcconstr (gsvar "lt") []
        EQ -> $gsbcconstr (gsvar "eq") []
        GT -> $gsbcconstr (gsvar "gt") []
    _ -> $gsbcimplementationfailure $ "gsnatural_eq " ++ gsvCode n0_0 ++ ' ' : gsvCode n1_0 ++ " next"

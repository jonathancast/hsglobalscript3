{-# LANGUAGE TemplateHaskell #-}
module GSI.Natural (gsnatural_unary_plus, gsnatural_add, gsnatural_addition, gsnatural_subtract, gsnatural_subtract_maybe, gsnatural_multiply, gsnatural_div_mod, gsnatural_eq, gsnatural_neq) where

import qualified Data.Map as Map

import GSI.Util (gshere)
import GSI.Syn (gsvar)
import GSI.Value (GSValue(..), gslambda_value, gsundefined_value, gsav, gsvCode)
import GSI.ByteCode (gsbcarg, gsbcforce, gsbcenter, gsbcnatural, gsbcconstr, gsbcrecord, gsbcimplementationfailure)

gsnatural_unary_plus = $gslambda_value $ \ n -> $gsbcenter n

gsnatural_add = $gslambda_value $ \ n0 -> $gsbcarg $ \ n1 -> $gsbcforce ($gsav n0) $ \ n0_0 -> $gsbcforce ($gsav n1) $ \ n1_0 -> case (n0_0, n1_0) of
    (GSNatural n0hs, GSNatural n1hs) -> $gsbcenter $ GSNatural $ n0hs + n1hs
    _ -> $gsbcimplementationfailure $ "gsnatural_add " ++ gsvCode n0_0 ++ ' ' : gsvCode n1_0 ++ " next"

gsnatural_addition = GSRecord $gshere $ Map.fromList [
    (gsvar "0", GSNatural 0),
    (gsvar "+", gsnatural_add)
  ]

gsnatural_subtract = $gslambda_value $ \ n0 -> $gsbcarg $ \ n1 -> $gsbcforce ($gsav n0) $ \ n0_0 -> $gsbcforce ($gsav n1) $ \ n1_0 -> case (n0_0, n1_0) of
    (GSNatural n0hs, GSNatural n1hs) -> let dhs = n0hs - n1hs in case dhs >= 0 of
        True -> $gsbcnatural dhs
        False -> $gsbcimplementationfailure $ "gsnatural_subtract: result is " ++ show dhs ++ " next"
    _ -> $gsbcimplementationfailure $ "gsnatural_subtract " ++ gsvCode n0_0 ++ ' ' : gsvCode n1_0 ++ " next"

gsnatural_subtract_maybe = $gslambda_value $ \ n0 -> $gsbcarg $ \ n1 -> $gsbcforce ($gsav n0) $ \ n0_0 -> $gsbcforce ($gsav n1) $ \ n1_0 -> case (n0_0, n1_0) of
    (GSNatural n0hs, GSNatural n1hs) -> let dhs = n0hs - n1hs in case dhs >= 0 of
        True -> $gsbcconstr (gsvar "just") [ $gsav $ GSNatural dhs ]
        False -> $gsbcconstr (gsvar "nothing") []
    _ -> $gsbcimplementationfailure $ "gsnatural_subtract_maybe " ++ gsvCode n0_0 ++ ' ' : gsvCode n1_0 ++ " next"

gsnatural_multiply = $gslambda_value $ \ n0 -> $gsbcarg $ \ n1 -> $gsbcforce ($gsav n0) $ \ n0_0 -> $gsbcforce ($gsav n1) $ \ n1_0 -> case (n0_0, n1_0) of
    (GSNatural n0hs, GSNatural n1hs) -> $gsbcenter $ GSNatural $ n0hs * n1hs
    _ -> $gsbcimplementationfailure $ "gsnatural_multiply " ++ gsvCode n0_0 ++ ' ' : gsvCode n1_0 ++ " next"

gsnatural_div_mod = $gslambda_value $ \ n0 -> $gsbcarg $ \ n1 -> $gsbcforce ($gsav n0) $ \ n0_0 -> $gsbcforce ($gsav n1) $ \ n1_0 -> case (n0_0, n1_0) of
    (GSNatural n0hs, GSNatural n1hs) -> case n1hs == 0 of
        False -> $gsbcrecord [ (gsvar "0", $gsav $ GSNatural $ n0hs `div` n1hs), (gsvar "1", $gsav $ GSNatural $ n0hs `mod` n1hs) ]
        True -> $gsbcimplementationfailure $ "gsnatural_div_mod n0hs 0 next"
    _ -> $gsbcimplementationfailure $ "gsnatural_div_mod " ++ gsvCode n0_0 ++ ' ' : gsvCode n1_0 ++ " next"

gsnatural_eq = $gslambda_value $ \ n0 -> $gsbcarg $ \ n1 -> $gsbcforce ($gsav n0) $ \ n0_0 -> $gsbcforce ($gsav n1) $ \ n1_0 -> case (n0_0, n1_0) of
    (GSNatural n0hs, GSNatural n1hs) ->
        if n0hs == n1hs then $gsbcconstr (gsvar "true") [] else $gsbcconstr (gsvar "false") []
    _ -> $gsbcimplementationfailure $ "gsnatural_eq " ++ gsvCode n0_0 ++ ' ' : gsvCode n1_0 ++ " next"

gsnatural_neq = $gslambda_value $ \ n0 -> $gsbcarg $ \ n1 -> $gsbcforce ($gsav n0) $ \ n0_0 -> $gsbcforce ($gsav n1) $ \ n1_0 -> case (n0_0, n1_0) of
    (GSNatural n0hs, GSNatural n1hs) ->
        if n0hs /= n1hs then $gsbcconstr (gsvar "true") [] else $gsbcconstr (gsvar "false") []
    _ -> $gsbcimplementationfailure $ "gsnatural_neq " ++ gsvCode n0_0 ++ ' ' : gsvCode n1_0 ++ " next"

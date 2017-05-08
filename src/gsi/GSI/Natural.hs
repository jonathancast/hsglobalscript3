{-# LANGUAGE TemplateHaskell #-}
module GSI.Natural (gsnatural_add) where

import GSI.Value (GSValue(..), gslambda, gsundefined_value, gsav, gsvCode)
import GSI.ByteCode (gsbcarg, gsbcforce, gsbcenter, gsbcundefined, gsbcimplementationfailure)

gsnatural_add = $gslambda $ \ n0 -> $gsbcarg $ \ n1 -> $gsbcforce ($gsav n0) $ \ n0_0 -> $gsbcforce ($gsav n1) $ \ n1_0 -> case (n0_0, n1_0) of
    (GSNatural n0hs, GSNatural n1hs) -> $gsbcenter $ GSNatural $ n0hs + n1hs
    _ -> $gsbcimplementationfailure $ "gsnatural_add " ++ gsvCode n0_0 ++ ' ' : gsvCode n1_0 ++ " next"

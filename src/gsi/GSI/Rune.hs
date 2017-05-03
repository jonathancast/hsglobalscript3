{-# LANGUAGE TemplateHaskell #-}
module GSI.Rune (gsruneEq) where

import GSI.Value (gslambda, gsundefined_value, gsav)
import GSI.ByteCode (gsbcarg, gsbcforce, gsbcundefined)

gsruneEq = $gslambda $ \ r0 -> $gsbcarg $ \ r1 -> $gsbcforce ($gsav r0) $ \ r0v -> $gsbcundefined

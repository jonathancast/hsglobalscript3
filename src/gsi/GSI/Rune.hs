{-# LANGUAGE TemplateHaskell #-}
module GSI.Rune (gsruneEq) where

import GSI.Syn (gsvar)
import GSI.Value (GSValue(..), gslambda_value, gsundefined_value, gsav, gsvCode)
import GSI.ByteCode (gsbcarg, gsbcforce, gsbcconstr, gsbcundefined, gsbcimplementationfailure)

gsruneEq = $gslambda_value $ \ r0 -> $gsbcarg $ \ r1 -> $gsbcforce ($gsav r0) $ \ r0v -> $gsbcforce ($gsav r1) $ \ r1v ->
    case (r0v, r1v) of
        (GSRune c0, GSRune c1)
            | c0 == c1 -> $gsbcconstr (gsvar "true") []
            | otherwise -> $gsbcconstr (gsvar "false") []
        _ -> $gsbcimplementationfailure $ "gsruneEq " ++ gsvCode r0v ++ ' ' : gsvCode r1v ++ " next"

{-# LANGUAGE TemplateHaskell #-}
module GSI.Log (gsbclog, gsbclog_w, gsbclogstring, gsbclogstring_w) where

import Language.Haskell.TH.Lib (varE, appE)

import GSI.Util (Pos, gshere)
import GSI.Value (GSValue(..), GSExpr, GSArg, gslambda, gsae, gsav)
import GSI.ByteCode (gsbcundefined, gsbcapply_w)
import GSI.StdLib (gscompose)

gsbclog = varE 'gsbclog_w `appE` gshere

gsbclog_w :: Pos -> [GSArg] -> GSExpr
gsbclog_w pos as = foldr (\ a k -> gsbcapply_w pos gscompose [ a, $gsae k ]) $gsbcundefined as

gslogchar :: GSValue
gslogchar = $gslambda $ \ ch -> $gsbcundefined

gsbclogstring = varE 'gsbclogstring_w `appE` gshere

gsbclogstring_w :: Pos -> String -> GSExpr
gsbclogstring_w pos s = foldr (\ ch k -> gsbcapply_w pos gscompose [ $gsae $ gsbcapply_w pos gslogchar [$gsav $ GSRune ch], $gsae $gsbcundefined ]) $gsbcundefined s

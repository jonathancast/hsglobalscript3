{-# LANGUAGE TemplateHaskell #-}
module GSI.Log (gsbclog, gsbclog_w, gsbclogstring, gsbclogstring_w) where

import Language.Haskell.TH.Lib (varE, appE)

import GSI.Util (Pos, gshere)
import GSI.Syn (gsvar)
import GSI.Value (GSValue(..), GSExpr, GSArg, gslambda, gsae, gsav)
import GSI.ByteCode (gsbcarg, gsbcundefined, gsbcenter, gsbcapply, gsbcapply_w, gsbcconstr, gsbcrecord, gsbcfield)
import GSI.StdLib (gscompose)
import GSI.List (gscons)

gsbclog = varE 'gsbclog_w `appE` gshere

gsbclog_w :: Pos -> [GSArg] -> GSExpr
gsbclog_w pos as = foldr (\ a k -> gsbcapply_w pos gscompose [ a, $gsae k ]) $gsbcundefined as

gslogchar :: GSValue
gslogchar = $gslambda $ \ ch -> $gsbcarg $ \ k -> $gsbcrecord [
    (gsvar "paragraph-constituents", $gsae $ $gsbcapply gscons [
        $gsae $ $gsbcconstr (gsvar "char") [ $gsav ch ],
        $gsae $ $gsbcfield (gsvar "paragraph-constituents") k $ \ r -> $gsbcenter r
    ])
  ]

gsbclogstring = varE 'gsbclogstring_w `appE` gshere

gsbclogstring_w :: Pos -> String -> GSExpr
gsbclogstring_w pos s = foldr (\ ch k -> gsbcapply_w pos gscompose [ $gsae $ gsbcapply_w pos gslogchar [$gsav $ GSRune ch], $gsae $gsbcundefined ]) $gsbcundefined s

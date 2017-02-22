{-# LANGUAGE TemplateHaskell #-}
module GSI.Log (gsbclog, gsbclog_w, gsbclogstring, gsbclogstring_w) where

import Language.Haskell.TH.Lib (varE, appE)

import GSI.Util (Pos, gshere)
import GSI.Value (GSExpr, GSArg, gsae)
import GSI.ByteCode (gsbcundefined, gsbcimplementationfailure_w, gsbcapply_w)
import GSI.StdLib (gscompose)

gsbclog = varE 'gsbclog_w `appE` gshere

gsbclog_w :: Pos -> [GSArg] -> GSExpr
gsbclog_w pos as = foldr
    (\ a k -> gsbcapply_w pos gscompose [ a, $gsae k ])
    (gsbcimplementationfailure_w $gshere $ "gsbclog_w next")
    as

gsbclogstring = varE 'gsbclogstring_w `appE` gshere

gsbclogstring_w :: Pos -> String -> GSExpr
gsbclogstring_w pos s = foldr (\ a k -> gsbcapply_w pos gscompose [ $gsae $gsbcundefined, $gsae $gsbcundefined ]) ($gsbcundefined) s

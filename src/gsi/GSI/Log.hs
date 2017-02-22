{-# LANGUAGE TemplateHaskell #-}
module GSI.Log (gsbclogstring, gsbclogstring_w) where

import Language.Haskell.TH.Lib (varE, appE)

import GSI.Util (Pos, gshere)
import GSI.Value (GSExpr, gsae)
import GSI.ByteCode (gsbcundefined, gsbcapply_w, gscompose)

gsbclogstring = varE 'gsbclogstring_w `appE` gshere

gsbclogstring_w :: Pos -> String -> GSExpr
gsbclogstring_w pos s = foldr (\ a k -> gsbcapply_w pos gscompose [ $gsae $gsbcundefined, $gsae $gsbcundefined ]) ($gsbcundefined) s

{-# LANGUAGE TemplateHaskell #-}
module GSI.Functions (gslist, gslist_w, gsstring, gsstring_w, gsnatural, gsnatural_w) where

import Language.Haskell.TH.Lib (appE, varE)

import GSI.Util (Pos, gshere)
import GSI.Syn (gsvar)
import GSI.Value (GSValue(..), gsundefined)

gslist = varE 'gslist_w `appE` gshere

gslist_w :: Pos -> [GSValue] -> GSValue
gslist_w pos [] = $gsundefined
gslist_w pos (x:xn) = GSConstr pos (gsvar ":") [ x, gslist_w pos xn ]

gsstring = varE 'gsstring_w `appE` gshere

gsstring_w :: Pos -> String -> GSValue
gsstring_w pos xn = $gsundefined

gsnatural = varE 'gsnatural_w `appE` gshere

gsnatural_w :: Pos -> Integer -> GSValue
gsnatural_w pos n = GSNatural n

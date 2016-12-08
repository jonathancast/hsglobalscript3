{-# LANGUAGE TemplateHaskell #-}
module GSI.Functions (gslist, gslist_w, gsstring, gsstring_w) where

import Language.Haskell.TH.Lib (appE, varE)

import GSI.Util (Pos, gshere)
import GSI.Value (GSValue, gsundefined)

gslist = varE 'gslist_w `appE` gshere

gslist_w :: Pos -> [GSValue] -> GSValue
gslist_w pos xn = $gsundefined

gsstring = varE 'gsstring_w `appE` gshere

gsstring_w :: Pos -> String -> GSValue
gsstring_w pos xn = $gsundefined

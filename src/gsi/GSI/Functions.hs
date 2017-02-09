{-# LANGUAGE TemplateHaskell #-}
module GSI.Functions (gslist, gslist_w, gsstring, gsstring_w, gsnatural, gsnatural_w, gsfmterrormsg) where

import Language.Haskell.TH.Lib (appE, varE)

import GSI.Util (Pos, gshere, fmtPos)
import GSI.Syn (gsvar)
import GSI.Value (GSValue(..), gsundefined, gsvCode)

gslist = varE 'gslist_w `appE` gshere

gslist_w :: Pos -> [GSValue] -> GSValue
gslist_w pos [] = GSConstr pos (gsvar "nil") []
gslist_w pos (x:xn) = GSConstr pos (gsvar ":") [ x, gslist_w pos xn ]

gsstring = varE 'gsstring_w `appE` gshere

gsstring_w :: Pos -> String -> GSValue
gsstring_w pos s = gslist_w pos $ map GSRune s

gsnatural = varE 'gsnatural_w `appE` gshere

gsnatural_w :: Pos -> Integer -> GSValue
gsnatural_w pos n = GSNatural n

gsfmterrormsg = varE 'gsfmterrormsg_w `appE` gshere

gsfmterrormsg_w :: Pos -> GSValue -> IO String
gsfmterrormsg_w pos msg = return $ fmtPos $gshere "gsfmterrormsg " ++ gsvCode msg ++ " next"

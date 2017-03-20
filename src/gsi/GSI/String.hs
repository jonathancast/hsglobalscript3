{-# LANGUAGE TemplateHaskell #-}
module GSI.String (gsbcstring, gsbcstring_w) where

import Language.Haskell.TH.Lib (appE, varE)

import GSI.Util (Pos, gshere)
import GSI.Syn (gsvar)
import GSI.Value (GSArg, GSExpr)
import GSI.ByteCode (gsbcconstr, gsbcimplementationfailure)

gsbcstring = varE 'gsbcstring_w `appE` gshere

gsbcstring_w :: Pos -> [GSArg] -> GSExpr
gsbcstring_w pos cs = gsbcstring_ww id cs where
    gsbcstring_ww :: (GSExpr -> GSExpr) -> [GSArg] -> GSExpr
    gsbcstring_ww ds (c:cs) = $gsbcimplementationfailure "gsbcstring_w next"
    gsbcstring_ww ds [] = ds $ $gsbcconstr (gsvar "nil") []

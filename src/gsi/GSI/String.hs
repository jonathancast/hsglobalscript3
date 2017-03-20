{-# LANGUAGE TemplateHaskell #-}
module GSI.String (gsbcstring, gsbcstring_w) where

import Language.Haskell.TH.Lib (appE, varE)

import GSI.Util (Pos, gshere)
import GSI.Syn (gsvar, fmtVarAtom)
import GSI.Value (GSValue(..), GSArg, GSExpr, gsae, gsav, gsvCode)
import GSI.ByteCode (gsbcapply, gsbcconstr, gsbcforce, gsbcimplementationfailure)
import GSI.List (gsappend)

gsbcstring = varE 'gsbcstring_w `appE` gshere

gsbcstring_w :: Pos -> [GSArg] -> GSExpr
gsbcstring_w pos cs = gsbcstring_ww id cs where
    gsbcstring_ww :: (GSExpr -> GSExpr) -> [GSArg] -> GSExpr
    gsbcstring_ww ds (c:cs) = gsbcstring_ww (ds . \ s1 -> $gsbcapply gsappend [ c, $gsae s1 ]) cs
    gsbcstring_ww ds [] = ds $ $gsbcconstr (gsvar "nil") []

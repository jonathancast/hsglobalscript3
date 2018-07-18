{-# LANGUAGE TemplateHaskell #-}
module GSI.String (gsbcstring, gsbcstring_w, gsbcstringlit, gsbcstringlit_w, gsfmtdecimal) where

import Language.Haskell.TH.Lib (appE, varE)

import GSI.Util (Pos, gshere)
import GSI.Syn (gsvar, fmtVarAtom)
import GSI.Value (GSValue(..), GSArg, GSExpr, gslambda_value, gsae, gsav, gsvCode)
import GSI.Functions (gsstring)
import GSI.ByteCode (gsbcapply, gsbcconstr, gsbcenter, gsbcenterarg, gsbcforce, gsbcevalnatural, gsbcimplementationfailure)
import GSI.List (gsappend, gscons, gsnil)

gsbcstring = varE 'gsbcstring_w `appE` gshere

gsbcstring_w :: Pos -> [GSArg] -> GSExpr
gsbcstring_w pos cs = gsbcstring_ww id cs where
    gsbcstring_ww :: (GSExpr -> GSExpr) -> [GSArg] -> GSExpr
    gsbcstring_ww ds [c] = ds $ $gsbcenterarg c
    gsbcstring_ww ds (c:cs) = gsbcstring_ww (ds . \ s1 -> $gsbcapply gsappend [ c, $gsae s1 ]) cs
    gsbcstring_ww ds [] = ds $ $gsbcconstr (gsvar "nil") []

gsbcstringlit = varE 'gsbcstringlit_w `appE` gshere

gsbcstringlit_w :: Pos -> String -> GSExpr
gsbcstringlit_w pos s = foldr (\ ch e1 -> $gsbcapply gscons [ $gsav $ GSRune ch, $gsae e1 ]) ($gsbcenter gsnil) s

gsfmtdecimal :: GSValue
gsfmtdecimal = $gslambda_value $ \ n -> $gsbcevalnatural ($gsav n) $ \ n_hs -> $gsbcenter ($gsstring (show n_hs))

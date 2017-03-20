{-# LANGUAGE TemplateHaskell #-}
module GSI.String (gsbcstring, gsbcstring_w, gsbcevalstring, gsbcevalstring_w) where

import Language.Haskell.TH.Lib (appE, varE)

import GSI.Util (Pos, gshere)
import GSI.Syn (gsvar, fmtVarAtom)
import GSI.Value (GSValue(..), GSArg, GSExpr, gsav, gsvCode)
import GSI.ByteCode (gsbcconstr, gsbcforce, gsbcimplementationfailure)

gsbcstring = varE 'gsbcstring_w `appE` gshere

gsbcstring_w :: Pos -> [GSArg] -> GSExpr
gsbcstring_w pos cs = gsbcstring_ww id cs where
    gsbcstring_ww :: (GSExpr -> GSExpr) -> [GSArg] -> GSExpr
    gsbcstring_ww ds (c:cs) = $gsbcimplementationfailure "gsbcstring_w next"
    gsbcstring_ww ds [] = ds $ $gsbcconstr (gsvar "nil") []

gsbcevalstring = varE 'gsbcevalstring_w `appE` gshere

gsbcevalstring_w :: Pos -> GSArg -> (String -> GSExpr) -> GSExpr
gsbcevalstring_w pos sa k = w id sa where
    w :: (String -> String) -> GSArg -> GSExpr
    w ds0 sa = $gsbcforce sa $ \ sv -> case sv of
        GSConstr _ s_c [ c0, s1 ] | s_c == gsvar ":" ->
            $gsbcforce ($gsav c0) $ \ c0v -> case c0v of
                GSRune c0_hs -> w (ds0 . (c0_hs:)) ($gsav s1)
                _ -> $gsbcimplementationfailure $ "gsbcevalstring_w (GSConstr (:) " ++ gsvCode c0v ++ ") next"
        GSConstr _ s_c s_as | s_c == gsvar "nil" -> k (ds0 [])
        GSConstr _ s_c s_as -> $gsbcimplementationfailure $ "gsbcevalstring_w (GSConstr " ++ fmtVarAtom s_c ") next"
        _ -> $gsbcimplementationfailure $ "gsbcevalstring_w " ++ gsvCode sv ++ " next"

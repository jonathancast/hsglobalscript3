{-# LANGUAGE TemplateHaskell #-}
module GSI.BCFunctions (gsbcevallist, gsbcevalmap) where

import Language.Haskell.TH.Lib (appE, varE)

import GSI.Util (Pos, gshere)
import GSI.Syn (gsvar, fmtVarAtom)
import GSI.Value (GSValue(..), GSExpr, GSArg, gsav, gsvCode)
import GSI.ByteCode (gsbcforce_w, gsbcimplementationfailure)

gsbcevalmap = varE 'gsbcevalmap_w `appE` gshere

gsbcevalmap_w :: Pos -> (GSArg -> (a -> GSExpr) -> GSExpr) -> [GSArg] -> ([a] -> GSExpr) -> GSExpr
gsbcevalmap_w pos f xn = foldr (\ aa b k -> f aa $ \ a -> b $ \ as -> k (a : as)) ($ []) xn

gsbcevallist = varE 'gsbcevallist_w `appE` gshere

gsbcevallist_w :: Pos -> GSArg -> ([GSValue] -> GSExpr) -> GSExpr

gsbcevallist_w pos a k = w a id where
    w a d = gsbcforce_w pos a $ \ v -> case v of
        GSConstr _ c [] | c == gsvar "nil" -> k (d [])
        GSConstr _ c [ v0, v1 ] | c == gsvar ":" -> w ($gsav v1) (d . (v0:))
        GSConstr _ c as -> $gsbcimplementationfailure $ "gsbcevallist " ++ fmtVarAtom c " next"
        _ -> $gsbcimplementationfailure $ "gsbcevallist " ++ gsvCode v ++ " next"

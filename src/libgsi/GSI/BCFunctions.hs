{-# LANGUAGE TemplateHaskell #-}
module GSI.BCFunctions (gsbcevalpos, gsbcevallist, gsbcevalmap, gsbcevalstring, gsbcevalstring_w) where

import Language.Haskell.TH.Lib (appE, varE)

import GSI.Util (Pos(..), gshere)
import GSI.Syn (gsvar, fmtVarAtom)
import GSI.Value (GSValue(..), GSExpr, GSArg, GSExternal(..), gsav, gsae, gsvCode)
import GSI.ByteCode (gsbcforce_w, gsbcevalnatural, gsbcfield, gsbcimplementationfailure)

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

gsbcevalpos = varE 'gsbcevalpos_w `appE` gshere

gsbcevalpos_w :: Pos -> GSArg -> (Pos -> GSExpr) -> GSExpr
gsbcevalpos_w pos pos1a k = gsbcforce_w pos pos1a $ \ pos1v -> case pos1v of
    GSRecord{} ->
        gsbcevalstring_w $gshere ($gsae $ $gsbcfield ($gsav pos1v) (gsvar "filename")) $ \ pos_filename_s ->
        $gsbcevalnatural ($gsae $ $gsbcfield ($gsav pos1v) (gsvar "line")) $ \ pos_line_n ->
        $gsbcevalnatural ($gsae $ $gsbcfield ($gsav pos1v) (gsvar "col")) $ \ pos_col_n ->
        k $ Pos pos_filename_s pos_line_n pos_col_n
    GSExternal e -> case fromExternal e of
        Nothing -> $gsbcimplementationfailure $ "gsbcevalpos_w (GSExternal (not a Pos)) next"
        Just pos -> k pos
    _ -> $gsbcimplementationfailure $ "gsbcevalpos_w " ++ gsvCode pos1v ++ " next"

gsbcevalstring = varE 'gsbcevalstring_w `appE` gshere

gsbcevalstring_w :: Pos -> GSArg -> (String -> GSExpr) -> GSExpr
gsbcevalstring_w pos sa k = w id sa where
    w :: (String -> String) -> GSArg -> GSExpr
    w ds0 sa = gsbcforce_w pos sa $ \ sv -> case sv of
        GSConstr _ s_c [ c0, s1 ] | s_c == gsvar ":" ->
            gsbcforce_w pos ($gsav c0) $ \ c0v -> case c0v of
                GSRune c0_hs -> w (ds0 . (c0_hs:)) ($gsav s1)
                _ -> $gsbcimplementationfailure $ "gsbcevalstring_w (GSConstr (:) " ++ gsvCode c0v ++ ") next"
        GSConstr _ s_c s_as | s_c == gsvar "nil" -> k (ds0 [])
        GSConstr _ s_c s_as -> $gsbcimplementationfailure $ "gsbcevalstring_w (GSConstr " ++ fmtVarAtom s_c ") next"
        _ -> $gsbcimplementationfailure $ "gsbcevalstring_w " ++ gsvCode sv ++ " next"

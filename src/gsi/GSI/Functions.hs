{-# LANGUAGE TemplateHaskell #-}
module GSI.Functions (gslist, gslist_w, gsstring, gsstring_w, gsnatural, gsnatural_w, gsfmterrormsg) where

import Language.Haskell.TH.Lib (appE, varE)

import GSI.Util (Pos, StackTrace(..), gshere, fmtPos)
import GSI.Syn (gsvar)
import GSI.Error (fmtError)
import GSI.Value (GSValue(..), gsundefined, gsimplementationfailure, gsapply, gsvCode)
import GSI.Eval (evalSync)

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
gsfmterrormsg_w pos msg = do
    msgs <- $gsapply msg [ $gsimplementationfailure "empty record next" ]
    gsfmterrormsg_ww pos id msgs

gsfmterrormsg_ww :: Pos -> (String -> String) -> GSValue -> IO String
gsfmterrormsg_ww pos ds (GSThunk th) = do
    v <- evalSync [StackTrace pos []] th
    gsfmterrormsg_ww pos ds v
gsfmterrormsg_ww pos ds (GSError err) = return $ (ds . ("<Error: "++) . (fmtError err++) . ('>':)) $ ""
gsfmterrormsg_ww pos0 ds (GSImplementationFailure pos1 msg) = return $ (ds . ("<Implementation Failure: "++) . (fmtPos pos1) . (msg++) . ('>':)) $ ""
gsfmterrormsg_ww pos ds msg =
    return $ (ds . ('<':) . fmtPos $gshere . ("gsfmterrormsg "++) . (gsvCode msg++) . (" next"++) . ('>':)) $ ""

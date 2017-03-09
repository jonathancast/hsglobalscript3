{-# LANGUAGE TemplateHaskell #-}
module GSI.Functions (gslist, gslist_w, gsstring, gsstring_w, gsnatural, gsnatural_w, gsfmterrormsg) where

import Language.Haskell.TH.Lib (appE, varE)

import qualified Data.Map as Map

import GSI.Util (Pos, StackTrace(..), gshere, fmtPos)
import GSI.Syn (gsvar, fmtVarAtom)
import GSI.Error (fmtError)
import GSI.Value (GSValue(..), gsundefined, gsimplementationfailure, gsapply, gsfield, gsvCode)
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
    msgt <- $gsapply msg [ GSRecord $gshere (Map.fromList [
        (gsvar "paragraph-constituents", GSConstr $gshere (gsvar "nil") [])
      ]) ]
    msg_pcs <- $gsfield (gsvar "paragraph-constituents") msgt
    gsfmterrormsg_ww pos id msg_pcs

gsfmterrormsg_ww :: Pos -> (String -> String) -> GSValue -> IO String
gsfmterrormsg_ww pos ds (GSThunk th) = do
    v <- evalSync [StackTrace pos []] th
    gsfmterrormsg_ww pos ds v
gsfmterrormsg_ww pos ds (GSError err) = return $ (ds . ("<Error: "++) . (fmtError err++) . ('>':)) $ ""
gsfmterrormsg_ww pos0 ds (GSImplementationFailure pos1 msg) = return $ (ds . ("<Implementation Failure: "++) . (fmtPos pos1) . (msg++) . ('>':)) $ ""
gsfmterrormsg_ww pos0 ds (GSConstr pos1 c [ GSThunk pcth, msg1 ]) | c == gsvar ":" = do
    pcv <- evalSync [StackTrace pos0 []] pcth
    gsfmterrormsg_ww pos0 ds (GSConstr pos1 c [ pcv, msg1 ])
gsfmterrormsg_ww pos0 ds (GSConstr pos1 c [ GSError err, msg1 ]) | c == gsvar ":" =
    gsfmterrormsg_ww pos0 (ds . ("<Error: "++) . (fmtError err++) . ('>':)) msg1
gsfmterrormsg_ww pos0 ds (GSConstr pos1 c1 [ GSConstr pos2 c2 [ GSRune ch ], msg1 ]) | c1 == gsvar ":" && c2 == gsvar "char" =
    gsfmterrormsg_ww pos0 (ds . (ch:)) msg1
gsfmterrormsg_ww pos0 ds (GSConstr pos1 c1 [ GSConstr pos2 c2 [ ch ], msg1 ]) | c1 == gsvar ":" && c2 == gsvar "char" =
    gsfmterrormsg_ww pos0 (ds . ('<':) . fmtPos $gshere . ("gsfmterrormsg (char "++) . (gsvCode ch++) . (" : _"++) . (") next"++) . ('>':)) msg1
gsfmterrormsg_ww pos0 ds (GSConstr pos1 c1 [ GSConstr pos2 c2 [ x ], msg1 ]) | c1 == gsvar ":" && c2 == gsvar "gsv" = do
    xds <- gsfmterrorvalue pos0 x
    gsfmterrormsg_ww pos0 (ds . xds) msg1
gsfmterrormsg_ww pos0 ds (GSConstr pos1 c1 [ GSConstr pos2 c2 _, msg1 ]) | c1 == gsvar ":" =
    gsfmterrormsg_ww pos0 (ds . ('<':) . fmtPos $gshere . ("gsfmterrormsg ("++) . fmtVarAtom c2 . (" : _"++) . (") next"++) . ('>':)) msg1
gsfmterrormsg_ww pos0 ds (GSConstr pos1 c [ c0, msg1 ]) | c == gsvar ":" =
    gsfmterrormsg_ww pos0 (ds . ('<':) . fmtPos $gshere . ("gsfmterrormsg ("++) . (gsvCode c0++) . (" : _"++) . (") next"++) . ('>':)) msg1
gsfmterrormsg_ww pos0 ds (GSConstr pos1 c []) | c== gsvar "nil" = return $ ds $ ""
gsfmterrormsg_ww pos0 ds (GSConstr pos1 c as) =
    return $ (ds . ('<':) . fmtPos $gshere . ("gsfmterrormsg "++) . fmtVarAtom c . (" next"++) . ('>':)) $ ""
gsfmterrormsg_ww pos ds msg =
    return $ (ds . ('<':) . fmtPos $gshere . ("gsfmterrormsg "++) . (gsvCode msg++) . (" next"++) . ('>':)) $ ""

-- §begin§itemize
--     §item Evaluates thunks
--     §item Prints errors
-- §end
gsfmterrorvalue :: Pos -> GSValue -> IO (String -> String)
gsfmterrorvalue pos (GSThunk th) = do
    v <- evalSync [StackTrace pos []] th
    gsfmterrorvalue pos v
gsfmterrorvalue pos v@GSImplementationFailure{} = gsfmterrorvalueAtom pos v
gsfmterrorvalue pos x = return $ ('<':) . fmtPos $gshere . ("gsfmterrorvalue "++) . (gsvCode x++) . (" next"++) . ('>':)

gsfmterrorvalueAtom :: Pos -> GSValue -> IO (String -> String)
gsfmterrorvalueAtom pos0 (GSImplementationFailure pos1 msg) = return $ ("<Implementation Failure: "++) . (fmtPos pos1) . (msg++) . ('>':)
gsfmterrorvalueAtom pos x = return $ ('<':) . fmtPos $gshere . ("gsfmterrorvalueAtom "++) . (gsvCode x++) . (" next"++) . ('>':)
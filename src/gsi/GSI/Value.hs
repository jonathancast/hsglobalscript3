{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module GSI.Value (GSValue(..), GSBCO(..), GSLambda(..), GSStackFrame(..), GSThunkState(..), gsundefined_w, gsapply, gsapply_w, gsundefined, gsimplementationfailure, gslambda, gslambda_w, gsthunk, gsthunk_w, gsvCode, bcoCode, gsstCode, gstsCode) where

import Control.Concurrent (MVar, newMVar)

import Language.Haskell.TH.Lib (appE, conE, varE)

import GSI.Util (Pos, gshere, gsfatal)
import GSI.Error (GSError(..))
import GSI.RTS (Event)
import GSI.Syn (GSVar)
import GSI.ThreadType (Thread)

data GSValue
  = GSImplementationFailure Pos String
  | GSError GSError
  | GSThunk GSThunk
  | GSClosure Pos GSBCO
  | GSLambda Pos (GSValue -> GSValue)
  | GSRawExpr ([GSStackFrame] -> IO GSValue)
  | GSConstr Pos GSVar [GSValue]

data GSBCO
  = GSBCOExpr ([GSStackFrame] -> IO GSValue) -- NB: return value is §emph{equal to} enclosing expression; computes and returns its own value
  | GSBCOImp (Thread -> IO GSValue) -- NB: return value §emph{result of} enclosing expression; computes and returns a different value
  | GSBCOVar Pos GSValue

data GSStackFrame
  = GSStackForce Pos (GSValue -> GSBCO)
  | GSStackArg Pos GSValue

type GSThunk = MVar GSThunkState

data GSThunkState
  = GSTSExpr ([GSStackFrame] -> IO GSValue)
  | GSApply Pos GSValue [GSValue]
  | GSTSStack Event
  | GSTSIndirection GSValue

gsundefined = varE 'gsundefined_w `appE` gshere
gsundefined_w pos = GSError (GSErrUnimpl pos)

gsimplementationfailure = conE 'GSImplementationFailure `appE` gshere

gsapply = varE 'gsapply_w `appE` gshere

gsapply_w pos fn args = fmap GSThunk $ newMVar $ GSApply pos fn args

gslambda = varE 'gslambda_w `appE` gshere

gslambda_w :: GSLambda bc => Pos -> (GSValue -> bc) -> GSValue
gslambda_w pos f = GSLambda pos (gslambda_ww pos . f)

class GSLambda r where
    gslambda_ww :: Pos -> r -> GSValue

instance GSLambda r => GSLambda (GSValue -> r) where
    gslambda_ww pos f = GSLambda pos (gslambda_ww pos . f)

instance GSLambda GSBCO where
    gslambda_ww pos bc@(GSBCOImp a) = GSClosure pos bc
    gslambda_ww pos (GSBCOExpr e) = GSRawExpr e
    gslambda_ww pos0 (GSBCOVar pos1 v) = v
    gslambda_ww pos bco = GSImplementationFailure $gshere $ "gslambda_ww " ++ bcoCode bco ++ " next"

gsthunk = varE 'gsthunk_w `appE` gshere

gsthunk_w :: Pos -> GSBCO -> IO GSValue
gsthunk_w pos bc = case bc of
    GSBCOExpr e -> fmap GSThunk $ newMVar $ GSTSExpr e
    GSBCOImp a -> return $ GSClosure pos $ GSBCOImp a
    GSBCOVar pos v -> return v
    bco -> return $ GSImplementationFailure $gshere $ "gsclosure_w " ++ bcoCode bco ++ " next"

gsvCode :: GSValue -> String
gsvCode GSImplementationFailure{} = "GSImplementationFailure"
gsvCode GSError{} = "GSError"
gsvCode GSThunk{} = "GSThunk"
gsvCode GSClosure{} = "GSClosure"
gsvCode GSLambda{} = "GSLambda"
gsvCode GSRawExpr{} = "GSRawExpr"
gsvCode GSConstr{} = "GSConstr"

bcoCode :: GSBCO -> String
bcoCode GSBCOExpr{} = "GSBCOExpr"
bcoCode GSBCOImp{} = "GSBCOImp"
bcoCode GSBCOVar{} = "GSBCOVar"

gsstCode :: GSStackFrame -> String
gsstCode GSStackForce{} = "GSStackForce"
gsstCode GSStackArg{} = "GSStackArg"

gstsCode :: GSThunkState -> String
gstsCode GSTSExpr{} = "GSTSExpr"
gstsCode GSApply{} = "GSApply"
gstsCode GSTSStack{} = "GSTSStack"
gstsCode GSTSIndirection{} = "GSTSIndirection"

{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module GSI.Value (GSValue(..), GSBCO(..), ToGSBCO(..), GSStackFrame(..), GSThunkState(..), gsundefined_w, gsapply, gsapply_w, gsundefined, gsimplementationfailure, gstoplevelclosure, gstoplevelclosure_w, gsclosure, gsclosure_w, gsvCode, bcoCode, gsstCode, gstsCode) where

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
  | GSConstr Pos GSVar [GSValue]

data GSBCO
  = GSBCOFun (GSValue -> GSBCO)
  | GSBCOExpr ([GSStackFrame] -> IO GSValue) -- NB: return value is §emph{equal to} enclosing expression; computes and returns its own value
  | GSBCOImp (Thread -> IO GSValue) -- NB: return value §emph{result of} enclosing expression; computes and returns a different value
  | GSBCOVar Pos GSValue

data GSStackFrame
  = GSStackForce Pos (GSValue -> GSBCO)
  | GSStackArg Pos GSValue

type GSThunk = MVar GSThunkState

data GSThunkState
  = GSTSExpr (IO GSValue)
  | GSApply Pos GSValue [GSValue]
  | GSTSStack Event
  | GSTSIndirection GSValue

gsundefined = varE 'gsundefined_w `appE` gshere
gsundefined_w pos = GSError (GSErrUnimpl pos)

gsimplementationfailure = conE 'GSImplementationFailure `appE` gshere

gsapply = varE 'gsapply_w `appE` gshere

gsapply_w pos fn args = fmap GSThunk $ newMVar $ GSApply pos fn args

gstoplevelclosure = varE 'gstoplevelclosure_w `appE` gshere

gstoplevelclosure_w :: ToGSBCO bc => Pos -> (GSValue -> bc) -> GSValue
gstoplevelclosure_w pos f = GSClosure pos (gsbco f)

gsclosure = varE 'gsclosure_w `appE` gshere

gsclosure_w :: ToGSBCO bc => Pos -> bc -> IO GSValue
gsclosure_w pos bc = case gsbco bc of
    GSBCOExpr e -> fmap GSThunk $ newMVar $ GSTSExpr (e [])
    GSBCOImp a -> return $ GSClosure pos $ GSBCOImp a
    GSBCOFun f -> return $ GSClosure pos $ GSBCOFun f
    GSBCOVar pos v -> return v
    bco -> return $ GSImplementationFailure $gshere $ "gsclosure_w " ++ bcoCode bco ++ " next"

class ToGSBCO r where
    gsbco :: r -> GSBCO

instance ToGSBCO r => ToGSBCO (GSValue -> r) where
    gsbco f = GSBCOFun (\ v -> gsbco (f v))

instance ToGSBCO GSBCO where
    gsbco = id

gsvCode :: GSValue -> String
gsvCode GSImplementationFailure{} = "GSImplementationFailure"
gsvCode GSError{} = "GSError"
gsvCode GSThunk{} = "GSThunk"
gsvCode GSClosure{} = "GSClosure"
gsvCode GSConstr{} = "GSConstr"

bcoCode :: GSBCO -> String
bcoCode GSBCOFun{} = "GSBCOFun"
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

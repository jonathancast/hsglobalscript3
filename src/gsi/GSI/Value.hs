{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module GSI.Value (GSValue(..), GSBCO(..), GSLambda(..), GSStackFrame(..), GSThunkState(..), GSBCImp(..), gsundefined_w, gsapply, gsapply_w, gsundefined, gsimplementationfailure, gslambda, gslambda_w, gsthunk, gsthunk_w, gsimpprim, gsimpprim_w, gsimpfor_w, gsvCode, bcoCode, gsstCode, gstsCode) where

import Control.Concurrent (MVar, newMVar)

import Language.Haskell.TH.Lib (appE, conE, varE)

import GSI.Util (Pos, StackTrace(..), gshere, gsfatal)
import GSI.Error (GSError(..))
import GSI.RTS (Event)
import GSI.Syn (GSVar)
import GSI.ThreadType (Thread)

data GSValue
  = GSImplementationFailure Pos String
  | GSError GSError
  | GSThunk GSThunk
  | GSLambda Pos (GSValue -> GSValue)
  | GSImp Pos (Thread -> IO GSValue)
  | GSRawExpr ([GSStackFrame] -> IO GSValue)
  | GSConstr Pos GSVar [GSValue]

data GSBCO
  = GSBCOExpr ([GSStackFrame] -> IO GSValue)
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

newtype GSBCImp a = GSBCImp { runGSBCImp :: Thread -> IO a }

instance Functor GSBCImp where
    fmap f ax = GSBCImp $ \ t -> fmap f $ runGSBCImp ax t

instance Applicative GSBCImp where
    pure x = GSBCImp (const $ pure x)
    af <*> ax = GSBCImp $ \ t -> runGSBCImp af t <*> runGSBCImp ax t

instance Monad GSBCImp where
    return x = GSBCImp (const $ return x)
    a >>= f = GSBCImp $ \ t -> runGSBCImp a t >>= \ x -> runGSBCImp (f x) t

gsimpfor = varE 'gsimpfor_w `appE` gshere

gsimpfor_w :: Pos -> GSBCImp GSValue -> GSValue
gsimpfor_w pos (GSBCImp a) = GSImp pos a

gsundefined = varE 'gsundefined_w `appE` gshere

gsundefined_w :: Pos -> GSValue
gsundefined_w pos = GSError (GSErrUnimpl (StackTrace pos []))

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
    gslambda_ww pos (GSBCOExpr e) = GSRawExpr e
    gslambda_ww pos0 (GSBCOVar pos1 v) = v
    gslambda_ww pos bco = GSImplementationFailure $gshere $ "gslambda_ww " ++ bcoCode bco ++ " next"

gsthunk = varE 'gsthunk_w `appE` gshere

gsthunk_w :: Pos -> GSBCO -> IO GSValue
gsthunk_w pos bc = case bc of
    GSBCOExpr e -> fmap GSThunk $ newMVar $ GSTSExpr e
    GSBCOVar pos v -> return v
    bco -> return $ GSImplementationFailure $gshere $ "gsclosure_w " ++ bcoCode bco ++ " next"

gsimpprim = varE 'gsimpprim_w `appE` gshere

gsimpprim_w :: GSImpPrimType f r => Pos -> (Pos -> Thread -> f) -> r
gsimpprim_w pos f = gsimpprim_ww pos (f pos)

class GSImpPrimType f r where
    gsimpprim_ww :: Pos -> (Thread -> f) -> r

instance GSImpPrimType (IO GSValue) GSValue where
    gsimpprim_ww pos f = GSImp pos f

gsvCode :: GSValue -> String
gsvCode GSImplementationFailure{} = "GSImplementationFailure"
gsvCode GSError{} = "GSError"
gsvCode GSThunk{} = "GSThunk"
gsvCode GSImp{} = "GSImp"
gsvCode GSLambda{} = "GSLambda"
gsvCode GSRawExpr{} = "GSRawExpr"
gsvCode GSConstr{} = "GSConstr"

bcoCode :: GSBCO -> String
bcoCode GSBCOExpr{} = "GSBCOExpr"
bcoCode GSBCOVar{} = "GSBCOVar"

gsstCode :: GSStackFrame -> String
gsstCode GSStackForce{} = "GSStackForce"
gsstCode GSStackArg{} = "GSStackArg"

gstsCode :: GSThunkState -> String
gstsCode GSTSExpr{} = "GSTSExpr"
gstsCode GSApply{} = "GSApply"
gstsCode GSTSStack{} = "GSTSStack"
gstsCode GSTSIndirection{} = "GSTSIndirection"

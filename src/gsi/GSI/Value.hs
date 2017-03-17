{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module GSI.Value (
    GSValue(..), GSBCO(..), GSExpr(..), GSArg(..), GSStackFrame(..), GSThunkState(..), GSBCImp(..),
    gsundefined_w, gsapply, gsapply_w, gsfield, gsfield_w, gsundefined, gsimplementationfailure, gslambda, gslambda_w,
    gsprepare, gsprepare_w, gsav, gsargvar, gsargvar_w, gsae, gsargexpr_w,
    gsthunk, gsthunk_w,
    gsimpprim, gsimpprim_w, gsimpfor_w,
    gsvCode, bcoCode, argCode, gsstCode, gstsCode
  ) where

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Concurrent (MVar, newMVar)

import Language.Haskell.TH.Lib (appE, conE, varE)

import GSI.Util (Pos, StackTrace(..), gshere, gsfatal)
import GSI.Error (GSError(..))
import GSI.RTS (Event)
import GSI.Syn (GSVar)
import GSI.ThreadType (Thread)

-- BIG IMPORTANT NOTE:
-- §begin§note
--     4 types!
--     §itemize
--         §item Values!  These are RRFs that have already been 'prepared'!  Some of them are thunks (§emph{shudder})!
--         §item Expressions!  These are IO code that calculates a value!  Basically byte-code!  GSI.ByteCode makes these!
--         §item Bodies!  These are things that can be bodies of closures!  Expressions or imp blocks or functions returning bodies!
--         §item Arguments!  These are things that can be RHSs of lets or applications!  Basically values or bodies!
-- §end

data GSValue
  = GSImplementationFailure Pos String
  | GSError GSError
  | GSThunk GSThunk
  | GSClosure [StackTrace] GSBCO
  | GSConstr Pos GSVar [GSValue]
  | GSRecord Pos (Map GSVar GSValue)
  | GSNatural Integer
  | GSRune Char

data GSBCO
  = GSRawExpr GSExpr
  | GSImp (Thread -> IO GSValue)
  | GSLambda (GSValue -> GSBCO)

data GSArg
  = GSArgExpr Pos GSExpr
  | GSArgVar GSValue

newtype GSExpr = GSExpr ([GSStackFrame] -> [StackTrace] -> IO GSValue)

data GSStackFrame
  = GSStackForce StackTrace (GSValue -> GSExpr)
  | GSStackArg StackTrace GSValue

type GSThunk = MVar GSThunkState

data GSThunkState
  = GSTSExpr ([GSStackFrame] -> [StackTrace] -> IO GSValue)
  | GSApply Pos GSValue [GSValue]
  | GSTSField Pos GSVar GSValue
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
gsimpfor_w pos (GSBCImp a) = GSClosure [StackTrace pos []] (GSImp a)

gsundefined = varE 'gsundefined_w `appE` gshere

gsundefined_w :: Pos -> GSValue
gsundefined_w pos = GSError (GSErrUnimpl (StackTrace pos []))

gsimplementationfailure = conE 'GSImplementationFailure `appE` gshere

gsapply = varE 'gsapply_w `appE` gshere

gsapply_w pos fn args = fmap GSThunk $ newMVar $ GSApply pos fn args

gslambda = varE 'gslambda_w `appE` gshere

gslambda_w :: Pos -> (GSValue -> GSExpr) -> GSValue
gslambda_w pos f = GSClosure [StackTrace pos []] (GSLambda (GSRawExpr . f)) where

gsfield = varE 'gsfield_w `appE` gshere

gsfield_w :: Pos -> GSVar -> GSValue -> IO GSValue
gsfield_w pos0 f r@GSThunk{} = fmap GSThunk $ newMVar $ GSTSField pos0 f r
gsfield_w pos0 f (GSRecord pos1 fs) = case Map.lookup f fs of
    Nothing -> return $ GSError $ GSErrUnimplField pos1 f
    Just v -> return v
gsfield_w pos0 f v@GSError{} = return v
gsfield_w pos0 f v@GSImplementationFailure{} = return v
gsfield_w pos0 f r = return $ GSImplementationFailure $gshere $ "gsfield " ++ gsvCode r ++ " next"

gsav = gsargvar
gsargvar = varE 'gsargvar_w `appE` gshere

gsargvar_w pos v = GSArgVar v

gsae = varE 'gsargexpr_w `appE` gshere

gsargexpr_w pos e = GSArgExpr pos e

gsthunk = varE 'gsthunk_w `appE` gshere

gsthunk_w :: Pos -> GSExpr -> IO GSValue
gsthunk_w pos (GSExpr e) = fmap GSThunk $ newMVar $ GSTSExpr e

gsprepare = varE 'gsprepare_w `appE` gshere

gsprepare_w :: Pos -> GSArg -> IO GSValue
gsprepare_w pos0 (GSArgExpr pos1 e) = gsthunk_w pos1 e
gsprepare_w pos0 (GSArgVar v) = return v
gsprepare_w pos a = return $ GSImplementationFailure $gshere $ "gsprepare_w " ++ argCode a ++ " next"

gsimpprim = varE 'gsimpprim_w `appE` gshere

gsimpprim_w :: GSImpPrimType f => Pos -> (Pos -> Thread -> f) -> GSValue
gsimpprim_w pos f = GSClosure [StackTrace pos []] (gsimpprim_ww (f pos))

class GSImpPrimType f where
    gsimpprim_ww :: (Thread -> f) -> GSBCO

instance GSImpPrimType (IO GSValue) where
    gsimpprim_ww f = GSImp f

gsvCode :: GSValue -> String
gsvCode GSImplementationFailure{} = "GSImplementationFailure"
gsvCode GSError{} = "GSError"
gsvCode GSThunk{} = "GSThunk"
gsvCode GSClosure{} = "GSClosure"
gsvCode GSConstr{} = "GSConstr"
gsvCode GSRecord{} = "GSRecord"
gsvCode GSNatural{} = "GSNatural"
gsvCode GSRune{} = "GSRune"

bcoCode :: GSBCO -> String
bcoCode GSRawExpr{} = "GSRawExpr"
bcoCode GSImp{} = "GSImp"
bcoCode GSLambda{} = "GSLambda"

argCode :: GSArg -> String
argCode GSArgExpr{} = "GSArgExpr"
argCode GSArgVar{} = "GSArgVar"

gsstCode :: GSStackFrame -> String
gsstCode GSStackForce{} = "GSStackForce"
gsstCode GSStackArg{} = "GSStackArg"

gstsCode :: GSThunkState -> String
gstsCode GSTSExpr{} = "GSTSExpr"
gstsCode GSApply{} = "GSApply"
gstsCode GSTSField{} = "GSTSField"
gstsCode GSTSStack{} = "GSTSStack"
gstsCode GSTSIndirection{} = "GSTSIndirection"
{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, Rank2Types, FlexibleInstances, ExistentialQuantification #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module GSI.Value (
    GSValue(..), GSValueEnv, GSThunk(..), GSBCO(..), GSExpr(..), GSIntArg(..), GSIntExpr(..), GSEvalState(..), GSExprCont(..), GSArg(..), GSThunkState(..), GSBCImp(..), GSExternal(..), Thread(..), ThreadState(..),
    gsundefined_value_w, gsapply, gsapply_w, gsfield, gsfield_w, gsconstr, gsrecord, gsrecord_w, gsundefined_value, gsimplementationfailure, gslambda_value, gslambda_w, gsexternal,
    gsprepare, gsprepare_w, gsintprepare, gsav, gsargvar_w, gsae, gsargexpr_w,
    gsthunk, gsthunk_w, gsintthunk_w,
    gsimpprim, gsimpprim_w, gsimpfor_w,
    gsrehere_w,
    gsvenvUnion,
    fmtExternal,
    gsvFmt, gsvCode, bcoCode, iexprCode, argCode, gstsCode, threadStateCode, whichExternal
  ) where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Functor.Identity (Identity(..))
import Data.Typeable (Typeable(..), cast, typeRep)

import Control.Concurrent (MVar, newMVar)

import Language.Haskell.TH.Lib (appE, conE, varE)

import GSI.Util (Pos, StackTrace(..), gshere, gsfatal)
import GSI.Error (GSError(..), GSInvalidProgram(..), errCode)
import GSI.Message (Message)
import GSI.Prof (ProfCounter)
import GSI.RTS (Event, OPort)
import GSI.Syn (GSVar, fmtVarAtom)

-- BIG IMPORTANT NOTE:
-- §begin§note
--     4 types!
--     §itemize
--         §item Values!  These are RRFs that have already been 'prepared'!  Some of them are thunks (§emph{shudder})!
--         §item Expressions!  These are IO code that calculates a value!  Basically byte-code!  GSI.ByteCode makes these!
--         §item Bodies!  These are things that can be bodies of closures!  Expressions or imp blocks or functions returning bodies!
--         §item Arguments!  These are things that can be RHSs of lets or applications!  Basically values or bodies!
-- §end

type GSValueEnv = Map GSVar GSValue

data GSValue
  = GSImplementationFailure Pos String
  | GSInvalidProgram GSInvalidProgram
  | GSError GSError
  | GSThunk GSThunk
  | GSClosure [StackTrace] GSBCO
  | GSConstr Pos GSVar [GSValue]
  | GSRecord Pos GSValueEnv
  | GSNatural [Message] Integer
  | GSRational Rational
  | GSRune Char
  | GSExternal SomeGSExternal

data GSBCO
  = GSRawExpr GSExpr
  | GSIntExpr GSIntExpr
  | GSImp (GSEvalState -> Thread -> IO GSValue)
  | GSLambda (GSValue -> GSBCO)

data GSIntArg
  = GSIArgExpr Pos GSIntExpr
  | GSIArgLVar GSVar
  | GSIArgGVar GSValue

data GSIntExpr
  = GSIntOpenExpr Pos GSIntExpr
  | GSIntWithHere Pos GSIntExpr
  | GSIntLField Pos Int GSVar GSIntExpr
  | GSIntPrim (IO GSValue)
  | GSIntGApply Pos GSValue [GSIntArg]
  | GSIntEApply Pos GSIntExpr [GSIntArg]
  | GSIntUndefined Pos
  | GSIntGEnter Pos GSValue
  | GSIntBEnter Pos GSExpr
  | GSIntFEnter Pos GSVar

data GSArg
  = GSArgExpr Pos GSExpr
  | GSArgVar GSValue

newtype GSExpr = GSExpr { runGSExpr :: forall a. GSEvalState -> [StackTrace] -> GSExprCont a -> IO a }

data GSExprCont a = GSExprCont {
    gsreturn :: GSValue -> IO a,
    gsthrow :: GSValue -> IO a
  }

type GSThunk = MVar GSThunkState

data GSEvalState = GSEvalState { msgChannel :: OPort Message, profCounter :: Maybe ProfCounter }

data GSThunkState
  = GSTSExpr (forall a. OPort Message -> Maybe ProfCounter -> [StackTrace] -> GSExprCont a -> IO a)
  | GSTSIntExpr GSIntExpr
  | GSApply Pos GSValue [GSValue]
  | GSTSField Pos GSVar GSValue
  | GSTSStack Event
  | GSTSIndirection GSValue

newtype GSBCImp a = GSBCImp { runGSBCImp :: GSEvalState -> Thread -> IO a }

instance Functor GSBCImp where
    fmap f ax = GSBCImp $ \ evs t -> fmap f $ runGSBCImp ax evs t

instance Applicative GSBCImp where
    pure x = GSBCImp (\ evs t -> pure x)
    af <*> ax = GSBCImp $ \ evs t -> runGSBCImp af evs t <*> runGSBCImp ax evs t

instance Monad GSBCImp where
    return x = GSBCImp (\ evs t -> return x)
    a >>= f = GSBCImp $ \ evs t -> runGSBCImp a evs t >>= \ x -> runGSBCImp (f x) evs t

data Thread = Thread {
    state :: MVar ThreadState,
    wait :: Event
  }

data ThreadState
  = ThreadStateRunning
  | ThreadStateInvalidProgram GSInvalidProgram
  | ThreadStateError GSError
  | ThreadStateImplementationFailure Pos String
  | ThreadStateAbend Pos String
  | ThreadStateSuccess

gsimpfor = varE 'gsimpfor_w `appE` gshere

gsimpfor_w :: Pos -> GSBCImp GSValue -> GSValue
gsimpfor_w pos (GSBCImp a) = GSClosure [StackTrace pos []] (GSImp a)

gsundefined_value = varE 'gsundefined_value_w `appE` gshere

gsundefined_value_w :: Pos -> GSValue
gsundefined_value_w pos = GSError (GSErrUnimpl (StackTrace pos []))

gsimplementationfailure = conE 'GSImplementationFailure `appE` gshere

gsapply = varE 'gsapply_w `appE` gshere

gsapply_w pos fn args = fmap GSThunk $ newMVar $ GSApply pos fn args

gslambda_value = varE 'gslambda_w `appE` gshere

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

gsconstr = conE 'GSConstr `appE` gshere

gsrecord = varE 'gsrecord_w `appE` gshere

gsrecord_w pos fs = GSRecord pos (Map.fromList fs)

gsav = varE 'gsargvar_w `appE` gshere

gsargvar_w pos v = GSArgVar v

gsae = varE 'gsargexpr_w `appE` gshere

gsargexpr_w pos e = GSArgExpr pos e

gsthunk = varE 'gsthunk_w `appE` gshere

gsthunk_w :: Pos -> GSExpr -> IO GSValue
gsthunk_w pos (GSExpr e) = fmap GSThunk $ newMVar $ GSTSExpr $ \ msg pc cs sk -> e (GSEvalState msg pc) (StackTrace pos [] : cs) sk

gsintthunk_w :: Pos -> GSIntExpr -> IO GSValue
gsintthunk_w pos i = fmap GSThunk $ newMVar $ GSTSIntExpr i

gsprepare = varE 'gsprepare_w `appE` gshere

gsprepare_w :: Pos -> GSArg -> IO GSValue
gsprepare_w pos0 (GSArgExpr pos1 e) = gsthunk_w pos1 e
gsprepare_w pos0 (GSArgVar v) = return v
gsprepare_w pos a = return $ GSImplementationFailure $gshere $ "gsprepare_w " ++ argCode a ++ " next"

gsintprepare :: GSIntArg -> IO GSValue
gsintprepare (GSIArgExpr pos e) = gsintthunk_w pos e
gsintprepare (GSIArgGVar v) = return v
gsintprepare a = return $ GSImplementationFailure $gshere $ "gsintprepare " ++ iargCode a ++ " next"

gsrehere_w :: Pos -> [StackTrace] -> GSValue -> GSValue
gsrehere_w pos cs (GSClosure _ b) = GSClosure [StackTrace pos cs] b
gsrehere_w pos cs (GSError (GSErrUnimpl st)) = GSError (GSErrUnimpl (StackTrace pos cs))
gsrehere_w pos cs (GSError (GSErrError _ msg)) = GSError (GSErrError pos msg)
gsrehere_w pos cs (GSError err) = GSImplementationFailure $gshere $ "gsrehere_w GSError (" ++ errCode err ++ ") next"
gsrehere_w pos cs v = GSImplementationFailure $gshere $ "gsrehere_w " ++ gsvCode v ++ " next"

gsimpprim = varE 'gsimpprim_w `appE` gshere

gsimpprim_w :: GSImpPrimType f => Pos -> (GSEvalState -> Pos -> Thread -> f) -> GSValue
gsimpprim_w pos f = GSClosure [StackTrace pos []] (gsimpprim_ww (\ evs -> f evs pos))

class GSImpPrimType f where
    gsimpprim_ww :: (GSEvalState -> Thread -> f) -> GSBCO

instance GSImpPrimType (IO GSValue) where
    gsimpprim_ww f = GSImp f

instance GSImpPrimType f => GSImpPrimType (GSValue -> f) where
    gsimpprim_ww f = GSLambda $ \ x -> gsimpprim_ww (\ evs t -> f evs t x)

gsexternal :: GSExternal e => e -> GSValue
gsexternal = GSExternal . toExternal

data SomeGSExternal = forall e. GSExternal e => SomeGSExternal e

class Typeable e => GSExternal e where
    toExternal   :: e -> SomeGSExternal
    fromExternal :: SomeGSExternal -> Maybe e

    toExternal = SomeGSExternal
    fromExternal (SomeGSExternal e) = cast e

    externalType :: proxy e -> String
    fmtExternal_w :: e -> IO (String -> String)

    externalType = show . typeRep
    fmtExternal_w e = return (externalType (Identity e)++)

fmtExternal :: SomeGSExternal -> IO (String -> String)
fmtExternal (SomeGSExternal e) = fmtExternal_w e

whichExternal :: SomeGSExternal -> String
whichExternal (SomeGSExternal e) = externalType (Identity e)

-- ↓ Instances that are here because we depend on the modules the types are in
instance GSExternal GSError
instance GSExternal GSVar
instance GSExternal Pos
instance GSExternal StackTrace
instance GSExternal Thread

-- ↓ Instances that are here because they go here
instance GSExternal GSArg
instance GSExternal GSExpr
instance GSExternal GSIntArg
instance GSExternal GSIntExpr
instance GSExternal GSBCO

instance GSExternal GSValue where
    fmtExternal_w v = return $ ("GSValue"++) . (' ':) . gsvFmt v

gsvenvUnion :: GSValueEnv -> GSValueEnv -> GSValueEnv
gsvenvUnion = Map.union

gsvFmt :: GSValue -> String -> String
gsvFmt (GSClosure _ bco) = ('(':) . ("GSClosure _ "++) . (bcoCode bco++) . (')':)
gsvFmt (GSConstr _ c as) = ('(':) . ("GSConstr _ "++) . fmtVarAtom c . (" _ "++) . (')':)
gsvFmt v = (gsvCode v++)

gsvCode :: GSValue -> String
gsvCode GSImplementationFailure{} = "GSImplementationFailure"
gsvCode GSInvalidProgram{} = "GSInvalidProgram"
gsvCode GSError{} = "GSError"
gsvCode GSThunk{} = "GSThunk"
gsvCode GSClosure{} = "GSClosure"
gsvCode GSConstr{} = "GSConstr"
gsvCode GSRecord{} = "GSRecord"
gsvCode GSNatural{} = "GSNatural"
gsvCode GSRational{} = "GSRational"
gsvCode GSRune{} = "GSRune"
gsvCode GSExternal{} = "GSExternal"

bcoCode :: GSBCO -> String
bcoCode GSRawExpr{} = "GSRawExpr"
bcoCode GSIntExpr{} = "GSIntExpr"
bcoCode GSImp{} = "GSImp"
bcoCode GSLambda{} = "GSLambda"

iargCode :: GSIntArg -> String
iargCode GSIArgExpr{} = "GSIArgExpr"
iargCode GSIArgLVar{} = "GSIArgLVar"
iargCode GSIArgGVar{} = "GSIArgGVar"

iexprCode :: GSIntExpr -> String
iexprCode GSIntOpenExpr{} = "GSIntOpenExpr"
iexprCode GSIntWithHere{} = "GSIntWithHere"
iexprCode GSIntLField{} = "GSIntLField"
iexprCode GSIntPrim{} = "GSIntPrim"
iexprCode GSIntGApply{} = "GSIntGApply"
iexprCode GSIntEApply{} = "GSIntEApply"
iexprCode GSIntUndefined{} = "GSIntUndefined"
iexprCode GSIntGEnter{} = "GSIntGEnter"
iexprCode GSIntBEnter{} = "GSIntBEnter"
iexprCode GSIntFEnter{} = "GSIntFEnter"

argCode :: GSArg -> String
argCode GSArgExpr{} = "GSArgExpr"
argCode GSArgVar{} = "GSArgVar"

gstsCode :: GSThunkState -> String
gstsCode GSTSExpr{} = "GSTSExpr"
gstsCode GSTSIntExpr{} = "GSTSIntExpr"
gstsCode GSApply{} = "GSApply"
gstsCode GSTSField{} = "GSTSField"
gstsCode GSTSStack{} = "GSTSStack"
gstsCode GSTSIndirection{} = "GSTSIndirection"

threadStateCode :: ThreadState -> String
threadStateCode ThreadStateRunning{} = "ThreadStateRunning"
threadStateCode ThreadStateInvalidProgram{} = "ThreadStateInvalidProgram"
threadStateCode ThreadStateError{} = "ThreadStateError"
threadStateCode ThreadStateImplementationFailure{} = "ThreadStateImplementationFailure"
threadStateCode ThreadStateAbend{} = "ThreadStateAbend"
threadStateCode ThreadStateSuccess{} = "ThreadStateSuccess"

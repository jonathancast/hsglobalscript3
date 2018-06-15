{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module GSI.Error (GSError(..), GSInvalidProgram(..), GSException(..), throwGSInvalidProgram, throwGSError, fmtInvalidProgram, fmtError, fmtErrorShort) where

import Data.Typeable (Typeable)

import Control.Exception (Exception(..), throw)

import GSI.Util (Pos, StackTrace(..), fmtPos, fmtStackTrace, gshere)
import GSI.Syn (GSVar, fmtVarAtom)

data GSError
  = GSErrUnimpl StackTrace
  | GSErrUnimplField Pos GSVar
  | GSErrInsufficientCases Pos String
  | GSErrError Pos String
  deriving (Show)

data GSInvalidProgram
  = GSIPRuntimeTypeError StackTrace String String String
  deriving (Show)

data GSException
  = GSExcUndefined StackTrace
  | GSExcInsufficientCases Pos String
  | GSExcError Pos String
  | GSExcRuntimeTypeError StackTrace String String String
  | GSExcImplementationFailure Pos String
  deriving (Typeable, Show)

instance Exception GSException where
    displayException (GSExcUndefined st) = fmtStackTrace st "Undefined"
    displayException (GSExcInsufficientCases pos err) = fmtPos pos $ "Missing case: " ++ err
    displayException (GSExcError pos err) = fmtPos pos $ "Error: " ++ err
    displayException (GSExcRuntimeTypeError st ctxt act exp) = fmtStackTrace st $ "In " ++ ctxt ++ ", found " ++ act ++ "; expected " ++ exp
    displayException (GSExcImplementationFailure pos err) = fmtPos pos err

throwGSInvalidProgram (GSIPRuntimeTypeError st ctxt act exp) = throw $ GSExcRuntimeTypeError st ctxt act exp

throwGSError (GSErrUnimpl st) = throw $ GSExcUndefined st
throwGSError (GSErrInsufficientCases pos err) = throw $ GSExcInsufficientCases pos err
throwGSError (GSErrError pos err) = throw $ GSExcError pos err
throwGSError err = throw $ GSExcImplementationFailure $gshere $ "throwGSerror (" ++ show err ++ ") next"

fmtInvalidProgram :: GSInvalidProgram -> String
fmtInvalidProgram (GSIPRuntimeTypeError st ctxt act exp) = fmtStackTrace st $ "In " ++ ctxt ++ ", found " ++ act ++ "; expected " ++ exp

fmtError :: GSError -> String
fmtError (GSErrUnimpl st) = fmtStackTrace st "Undefined"
fmtError (GSErrUnimplField pos f) = fmtPos pos . ("Undefined field "++) . fmtVarAtom f $ ""
fmtError (GSErrInsufficientCases pos err) = fmtPos pos $ "Missing case: " ++ err
fmtError (GSErrError pos err) = fmtPos pos $ "Error: " ++ err

fmtErrorShort :: GSError -> String
fmtErrorShort (GSErrUnimpl (StackTrace pos _)) = fmtPos pos "Undefined"
fmtErrorShort (GSErrUnimplField pos f) = fmtPos pos . ("Undefined field "++) . fmtVarAtom f $ ""
fmtErrorShort (GSErrInsufficientCases pos err) = fmtPos pos $ "Missing case: " ++ err
fmtErrorShort (GSErrError pos err) = fmtPos pos $ "Error: " ++ err

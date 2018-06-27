{-# LANGUAGE TemplateHaskell, ExistentialQuantification, Rank2Types, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module GSI.ThreadType (Thread(..), ThreadState(..), ThreadException(..), threadStateCode) where

import qualified Data.Map as Map

import Data.Map (Map)
import Data.Typeable (Typeable, TypeRep)

import Control.Concurrent.MVar (MVar)
import Control.Exception (Exception(..))

import GSI.Util (Pos, gsfatal, fmtPos)
import GSI.RTS (Event)
import GSI.Error (GSInvalidProgram, GSError, fmtInvalidProgram, fmtError)

data Thread = Thread {
    state :: MVar ThreadState,
    wait :: Event
  }

data ThreadState
  = ThreadStateRunning
  | ThreadStateInvalidProgram GSInvalidProgram
  | ThreadStateError GSError
  | ThreadStateImplementationFailure Pos String
  | ThreadStateUnimpl Pos String
  | ThreadStateSuccess

data ThreadException
  = TEImplementationFailure Pos String
  | TEInvalidProgram GSInvalidProgram
  | TEError GSError
  deriving Show

instance Exception ThreadException where
    displayException (TEImplementationFailure pos err) = fmtPos pos err
    displayException (TEInvalidProgram err) = fmtInvalidProgram err
    displayException (TEError err) = fmtError err

threadStateCode :: ThreadState -> String
threadStateCode ThreadStateRunning{} = "ThreadStateRunning"
threadStateCode ThreadStateInvalidProgram{} = "ThreadStateInvalidProgram"
threadStateCode ThreadStateError{} = "ThreadStateError"
threadStateCode ThreadStateImplementationFailure{} = "ThreadStateImplementationFailure"
threadStateCode ThreadStateUnimpl{} = "ThreadStateUnimpl"
threadStateCode ThreadStateSuccess{} = "ThreadStateSuccess"

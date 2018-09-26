{-# LANGUAGE TemplateHaskell, ExistentialQuantification, Rank2Types, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module GSI.ThreadType (Thread(..), ThreadState(..), threadStateCode) where

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
  | ThreadStateSuccess

threadStateCode :: ThreadState -> String
threadStateCode ThreadStateRunning{} = "ThreadStateRunning"
threadStateCode ThreadStateInvalidProgram{} = "ThreadStateInvalidProgram"
threadStateCode ThreadStateError{} = "ThreadStateError"
threadStateCode ThreadStateImplementationFailure{} = "ThreadStateImplementationFailure"
threadStateCode ThreadStateSuccess{} = "ThreadStateSuccess"

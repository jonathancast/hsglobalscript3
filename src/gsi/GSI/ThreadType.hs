{-# LANGUAGE TemplateHaskell, ExistentialQuantification, Rank2Types #-}
module GSI.ThreadType (Thread(..), ThreadState(..), ThreadData(..), ThreadDataComponent(..), ThreadException(..), fetchThreadDataComponent, threadStateCode) where

import Data.Map (Map)
import Data.Typeable (TypeRep)

import Control.Concurrent.MVar (MVar)
import Control.Exception (Exception(..))

import Component.Monad (MonadComponentImpl)

import GSI.Util (Pos, gsfatal, fmtPos)
import GSI.RTS (Event)
import GSI.Error (GSError, fmtError)

data Thread = forall d. ThreadData d => Thread {
    state :: MVar ThreadState,
    threadData :: d,
    wait :: Event
  }

data ThreadState
  = ThreadStateRunning
  | ThreadStateError GSError
  | ThreadStateImplementationFailure Pos String
  | ThreadStateUnimpl Pos String

newtype ThreadDataComponents d = ThreadDataComponents (Map TypeRep (ThreadDataComponentWrapper d))

data ThreadDataComponentWrapper d = forall a. ThreadDataComponent a => ThreadDataComponentWrapper (forall b. MonadComponentImpl IO b a)

fetchThreadDataComponent :: ThreadDataComponent a => ThreadDataComponents d -> d -> Maybe (MonadComponentImpl IO b a)
fetchThreadDataComponent = $gsfatal "fetchThreadDataComponent next"

class ThreadDataComponent a where

class ThreadData d where
    component :: ThreadDataComponent a => d -> Maybe (MonadComponentImpl IO b a)
    threadTypeName :: d -> String

instance ThreadData () where
    component _ = Nothing
    threadTypeName _ = "()"

data ThreadException
  = TEImplementationFailure Pos String
  | TEError GSError
  deriving Show

instance Exception ThreadException where
    displayException (TEImplementationFailure pos err) = fmtPos pos err
    displayException (TEError err) = fmtError err

threadStateCode :: ThreadState -> String
threadStateCode ThreadStateRunning{} = "ThreadStateRunning"
threadStateCode ThreadStateError{} = "ThreadStateError"
threadStateCode ThreadStateImplementationFailure{} = "ThreadStateImplementationFailure"
threadStateCode ThreadStateUnimpl{} = "ThreadStateUnimpl"

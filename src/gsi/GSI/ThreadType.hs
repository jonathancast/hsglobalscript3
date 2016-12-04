module GSI.ThreadType (Thread(..), ThreadState(..), ThreadData(..), ThreadDataComponent(..), ThreadException(..), threadStateCode) where

import Control.Concurrent.MVar (MVar)
import Control.Exception (Exception(..))

import Component.Monad (MonadComponentImpl)

import GSI.Util (Pos, fmtPos)
import GSI.RTS (Event)
import GSI.Error (GSError, fmtError)

data Thread = Thread {
    state :: MVar ThreadState,
    wait :: Event
  }

data ThreadState
  = ThreadStateRunning
  | ThreadStateError GSError
  | ThreadStateImplementationFailure Pos String
  | ThreadStateUnimpl Pos String

class ThreadDataComponent a where

class ThreadData d where
    component :: ThreadDataComponent a => d -> Maybe (MonadComponentImpl IO b a) -- accesses a component of a state object, §emph{if such a component exists}
    threadTypeName :: d -> String

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

{-# LANGUAGE TemplateHaskell, ExistentialQuantification, Rank2Types, ScopedTypeVariables #-}
module GSI.ThreadType (Thread(..), ThreadState(..), ThreadData(..), ThreadDataComponent(..), ThreadException(..), fetchThreadDataComponent, insertThreadDataComponent, emptyThreadDataComponents, simpleThreadData, threadStateCode) where

import qualified Data.Map as Map

import Data.Map (Map)
import Data.Typeable (Typeable, TypeRep, Proxy(..), gcast, typeRep)

import Control.Concurrent.MVar (MVar)
import Control.Exception (Exception(..))

import Component.Monad (MonadComponentImpl, MonadComponentWrapper(..))

import GSI.Util (Pos, gsfatal, fmtPos)
import GSI.RTS (Event)
import GSI.Error (GSError, fmtError)

data Thread = Thread {
    state :: MVar ThreadState,
    threadData :: ThreadData,
    wait :: Event
  }

data ThreadState
  = ThreadStateRunning
  | ThreadStateError GSError
  | ThreadStateImplementationFailure Pos String
  | ThreadStateUnimpl Pos String

newtype ThreadDataComponents d = ThreadDataComponents (Map TypeRep (d -> ThreadDataComponentWrapper))

data ThreadDataComponentWrapper = forall a. ThreadDataComponent a => ThreadDataComponentWrapper (MonadComponentWrapper IO a)

fetchThreadDataComponent :: forall d a b. ThreadDataComponent a => ThreadDataComponents d -> d -> Maybe (MonadComponentImpl IO b a)
fetchThreadDataComponent (ThreadDataComponents m) d = do
    w <- Map.lookup (typeRep (Proxy :: Proxy a)) m
    case w d of
        ThreadDataComponentWrapper cw -> do
            cw' <- gcast cw
            case cw' of
                MonadComponentWrapper c -> return c

insertThreadDataComponent :: forall d a. ThreadDataComponent a => (forall b. d -> MonadComponentImpl IO b a) -> ThreadDataComponents d -> ThreadDataComponents d
insertThreadDataComponent cf (ThreadDataComponents m) = ThreadDataComponents $
    Map.insert
        (typeRep (Proxy :: Proxy a))
        (\ d -> ThreadDataComponentWrapper (MonadComponentWrapper (cf d)))
        m

emptyThreadDataComponents :: ThreadDataComponents d
emptyThreadDataComponents = ThreadDataComponents Map.empty

class Typeable a => ThreadDataComponent a where

data ThreadData = ThreadData {
    component :: forall a b. ThreadDataComponent a => Maybe (MonadComponentImpl IO b a),
    threadTypeName :: String
  }

simpleThreadData = ThreadData {
    component = Nothing,
    threadTypeName = "()"
  }

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

{-# LANGUAGE TemplateHaskell, ExistentialQuantification, Rank2Types, ScopedTypeVariables #-}
module GSI.ThreadType (Thread(..), ThreadState(..), ThreadData(..), ThreadDataComponent(..), ThreadException(..), fetchThreadDataComponent, emptyThreadDataComponents, threadStateCode) where

import qualified Data.Map as Map

import Data.Map (Map)
import Data.Typeable (Typeable, TypeRep, Proxy(..), gcast, typeRep)

import Control.Concurrent.MVar (MVar)
import Control.Exception (Exception(..))

import Component.Monad (MonadComponentImpl, MonadComponentWrapper(..))

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

emptyThreadDataComponents :: ThreadDataComponents d
emptyThreadDataComponents = ThreadDataComponents Map.empty

class Typeable a => ThreadDataComponent a where

class Typeable d => ThreadData d where
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

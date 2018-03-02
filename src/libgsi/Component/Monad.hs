{-# LANGUAGE Rank2Types #-}
module Component.Monad (MonadComponentImpl, MonadComponentWrapper(..), getM, mvarContents) where

import Control.Concurrent.MVar (MVar, modifyMVar)

type MonadComponent m a = forall b. MonadComponentImpl m b a

type MonadComponentImpl m b a = (a -> (b, a)) -> m b

data MonadComponentWrapper m a = MonadComponentWrapper (forall b. MonadComponentImpl m b a)

getM :: MonadComponentImpl m a a -> m a
getM l = l $ \ x -> (x, x)

mvarContents :: MVar a -> MonadComponent IO a
mvarContents mv f = modifyMVar mv (\ x -> let (y, x') = f x in return (x', y))

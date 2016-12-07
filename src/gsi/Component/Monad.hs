{-# LANGUAGE Rank2Types #-}
module Component.Monad (MonadComponentImpl, MonadComponentWrapper(..), getM) where

type MonadComponentImpl m b a = (a -> (b, a)) -> m b

data MonadComponentWrapper m a = MonadComponentWrapper (forall b. MonadComponentImpl m b a)

getM :: MonadComponentImpl m a a -> m a
getM l = l $ \ x -> (x, x)

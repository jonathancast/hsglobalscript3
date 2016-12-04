module Component.Monad (MonadComponentImpl, getM) where

type MonadComponentImpl m b a = (a -> (b, a)) -> m b

getM :: MonadComponentImpl m a a -> m a
getM l = l $ \ x -> (x, x)

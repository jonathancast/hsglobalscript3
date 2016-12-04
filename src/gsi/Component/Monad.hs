module Component.Monad (MonadComponentImpl) where

type MonadComponentImpl m b a = (a -> (b, a)) -> m b

module Mean.Walk where

import Control.Monad.Identity

class Walkable a where
  walkM' :: Monad m => (a -> m a) -> (a -> m a) -> a -> m a

  walkM :: Monad m => (a -> m a) -> a -> m a
  walkM f = walkM' f f

  walk' :: (a -> a) -> (a -> a) -> a -> a
  walk' f g = runIdentity . walkM' (pure . f) (pure . g)

  walk :: (a -> a) -> a -> a
  walk f = walk' f f

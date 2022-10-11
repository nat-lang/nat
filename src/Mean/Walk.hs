module Mean.Walk where

import Control.Monad.Identity

class Walkable a where
  preOrderM :: Monad m => (a -> m a) -> a -> m a

  walk :: (a -> a) -> a -> a
  walk f = runIdentity . preOrderM (return . f)

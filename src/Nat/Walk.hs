{-# LANGUAGE MultiParamTypeClasses #-}

module Nat.Walk where

import Control.Monad ((<=<))
import Control.Monad.Identity

class Walkable a where
  walkMC' ::
    Monad m =>
    (a -> (a -> m a) -> m a) ->
    a ->
    m a

  walkC' :: (a -> (a -> a) -> a) -> a -> a
  walkC' f = runIdentity . walkMC' f'
    where
      f' e c = pure $ f e (runIdentity . c)

  walkC :: (a -> (a -> a) -> a) -> a -> a
  walkC f = walkC' f

  walkM' :: Monad m => (a -> m a) -> a -> m a
  walkM' f = walkMC' (\e c -> (c <=< f) e)

  walkM :: Monad m => (a -> m a) -> a -> m a
  walkM f = walkM' f

  walk' :: (a -> a) -> a -> a
  walk' f = runIdentity . walkM' (pure . f)

  walk :: (a -> a) -> a -> a
  walk f = walk' f

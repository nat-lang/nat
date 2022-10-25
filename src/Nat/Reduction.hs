{-# LANGUAGE FunctionalDependencies #-}

module Nat.Reduction where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (ReaderT (runReaderT))

type Reduction a e r = ReaderT r (ExceptT e Identity) a

-- | Reduce an `a` to a `b` in environment `r` with exceptions `e`.
--   The type `a` uniquely determines `b` and `r`.
class Reducible a b e r | a -> b, a -> r where
  reduce :: a -> Reduction b e r
  runReduce :: a -> Either e b
  runReduce' :: r -> a -> Either e b
  runReduce' env a = runIdentity $ runExceptT (runReaderT (reduce a) env)

  normalize :: a -> Reduction b e r
  runNormalize :: a -> Either e b
  runNormalize' :: r -> a -> Either e b
  runNormalize' env a = runIdentity $ runExceptT (runReaderT (normalize a) env)

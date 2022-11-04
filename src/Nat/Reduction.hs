{-# LANGUAGE FunctionalDependencies #-}

module Nat.Reduction where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (ReaderT (runReaderT))

type Reduction a e r = ReaderT r (ExceptT e Identity) a

runReduction :: Reduction a e r -> r -> Either e a
runReduction m env = runIdentity $ runExceptT (runReaderT m env)

class Reducible a b e r | a -> r where
  reduce :: a -> Reduction b e r
  runReduce :: a -> Either e b
  runReduce' :: r -> a -> Either e b
  runReduce' env a = runReduction (reduce a) env

  normalize :: a -> Reduction b e r
  runNormalize :: a -> Either e b
  runNormalize' :: r -> a -> Either e b
  runNormalize' env a = runReduction (normalize a) env

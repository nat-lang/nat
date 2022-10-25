{-# LANGUAGE RankNTypes #-}

module Nat.Cata where

import Control.Monad ((<=<))

type Algebra f a = f a -> a

type CoAlgebra f a = a -> f a

type MonadicAlgebra f m a = Monad m => f a -> m a

type MonoidalAlgebra f m a = Monoid m => f a -> m

newtype Mu f = In {out :: f (Mu f)}

cata :: Functor f => (f a -> a) -> Mu f -> a
cata alg = alg . fmap (cata alg) . out

-- cata f (In x) = f (fmap (cata f) x)

cataM ::
  (Traversable f, Monad m) =>
  MonadicAlgebra f m a ->
  (Mu f -> m a)
cataM f = f <=< mapM (cataM f) . out

ana :: Functor f => CoAlgebra f a -> (a -> Mu f)
ana coAlg = In . fmap (ana coAlg) . coAlg

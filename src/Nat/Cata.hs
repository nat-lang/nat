{-# LANGUAGE RankNTypes #-}

module Nat.Cata where

import Control.Monad ((<=<), (>=>))
import Control.Monad.Cont
import Control.Monad.Identity

type Algebra f a = f a -> a

type CoAlgebra f a = a -> f a

type MonadicAlgebra f m a = Monad m => f a -> m a

type MonadicCoAlgebra f m a = a -> m (f a)

type MonoidalAlgebra f m a = Monoid m => f a -> m

newtype Mu f = In (f (Mu f))

out :: Mu f -> f (Mu f)
out (In x) = x

cata :: Functor f => Algebra f a -> Mu f -> a
cata f = f . fmap (cata f) . out

ana :: Functor f => (a -> f a) -> a -> Mu f
ana f = In . fmap (ana f) . f

cataM ::
  (Traversable f, Monad m) =>
  MonadicAlgebra f m a ->
  (Mu f -> m a)
cataM f = f <=< mapM (cataM f) . out

anaM ::
  (Traversable f, Monad m) =>
  MonadicCoAlgebra f m a ->
  a ->
  m (Mu f)
anaM f = f >=> fmap In . mapM (anaM f)

anaC :: Traversable f => (a -> (f a -> r) -> r) -> a -> (Mu f -> r) -> r
anaC f e = runCont $ anaM (cont . f) e

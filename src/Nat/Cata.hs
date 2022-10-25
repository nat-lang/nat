{-# LANGUAGE RankNTypes #-}

module Nat.Cata where

import Control.Monad ((<=<), (>=>))
import Control.Monad.Cont (ContT, MonadCont, callCC, cont, runCont)
import Control.Monad.Identity (Identity)

type Algebra f a = f a -> a

type CoAlgebra f a = a -> f a

type MonadicAlgebra f m a = Monad m => f a -> m a

type MonadicCoAlgebra f m a = a -> m (f a)

type MonoidalAlgebra f m a = Monoid m => f a -> m

newtype Mu f = In (f (Mu f))

out :: Mu f -> f (Mu f)
out (In x) = x

cata :: Functor f => Algebra f a -> Mu f -> a
cata alg = alg . fmap (cata alg) . out

ana :: Functor f => (a -> f a) -> a -> Mu f
ana alg = In . fmap (ana alg) . alg

cataM ::
  (Traversable f, Monad m) =>
  MonadicAlgebra f m a ->
  (Mu f -> m a)
cataM alg = alg <=< mapM (cataM alg) . out

anaM ::
  (Traversable f, Monad m) =>
  MonadicCoAlgebra f m a ->
  a ->
  m (Mu f)
anaM f = f >=> fmap In . mapM (anaM f)

anaC ::
  Traversable f =>
  ((f p -> ContT r Identity b) -> ContT r Identity (f p)) ->
  (p -> (Mu f -> r) -> r)
anaC alg = runCont . anaM alg'
  where
    alg' e = callCC alg
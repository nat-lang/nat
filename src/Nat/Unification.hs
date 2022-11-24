{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Nat.Unification where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Identity (Identity, runIdentity)
import Data.Either (isRight)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace (traceM)
import Nat.Context
import Prelude hiding ((<*>))

data UnificationError a
  = NotUnifiable !a !a
  deriving (Eq, Show)

type UnifyM a = ExceptT (UnificationError a) Identity (Env a)

class (Substitutable a a, Show a) => Unifiable a where
  unify :: a -> a -> UnifyM a

  unifyMany' :: [Pair a] -> UnifyM a
  unifyMany' cs = case cs of
    [] -> pure mempty
    ((a0, a1) : cs') -> do
      u <- unify a0 a1
      us <- unifyMany' (inEnv u cs')
      return (u <.> us)

  unifyMany :: [Pair a] -> UnifyM a
  unifyMany = unifyMany'

  runUnify :: [Pair a] -> Either (UnificationError a) (Env a)
  runUnify = runIdentity . runExceptT . unifyMany

  unifiable :: a -> a -> Bool
  unifiable t0 t1 = isRight $ runIdentity $ runExceptT $ unify t0 t1

  (<=>) :: a -> a -> Bool
  (<=>) = unifiable

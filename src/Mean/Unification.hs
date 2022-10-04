{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Mean.Unification where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Identity (Identity, runIdentity)
import Data.Either (isRight)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Mean.Var
import Prelude hiding ((<*>))

data UnificationError a
  = NotUnifiable !a !a
  deriving (Eq, Show)

type Env a = Map.Map Var a

type UnifyM a = ExceptT (UnificationError a) Identity (Env a)

type Pair a = (a, a)

mkEnv :: Var -> a -> Env a
mkEnv = Map.singleton

class Substitutable a b where
  substitute :: Env a -> b -> b

  inEnv :: Var -> a -> b -> b
  inEnv v a = substitute (mkEnv v a)

class Contextual a where
  fv :: a -> Set.Set Var

instance Substitutable a b => Substitutable a (Pair b) where
  substitute :: Substitutable a b => Env a -> Pair b -> Pair b
  substitute s (a0, a1) = (substitute s a0, substitute s a1)

instance Substitutable a b => Substitutable a [b] where
  substitute :: Env a -> [b] -> [b]
  substitute = map . substitute

instance Substitutable a b => Substitutable a (Env b) where
  substitute :: Substitutable a b => Env a -> Env b -> Env b
  substitute s env = Map.map (substitute s) env

instance Contextual a => Contextual [a] where
  fv :: [a] -> Set.Set Var
  fv = foldr (Set.union . fv) Set.empty

instance Contextual a => Contextual (a, a) where
  fv :: Contextual a => (a, a) -> Set.Set Var
  fv (t0, t1) = fv t0 `Set.union` fv t1

instance Contextual a => Contextual (Env a) where
  fv :: Contextual a => Env a -> Set.Set Var
  fv env = fv $ Map.elems env

(<*>) :: Substitutable a a => Env a -> Env a -> Env a
(<*>) e0 e1 = Map.map (substitute e0) e1 `Map.union` e0

class Substitutable a a => Unifiable a where
  unify :: a -> a -> UnifyM a

  unifyMany :: [Pair a] -> UnifyM a
  unifyMany cs = case cs of
    [] -> pure mempty
    ((a0, a1) : cs') -> do
      u <- unify a0 a1
      us <- unifyMany (substitute u cs')
      pure $ u <*> us

  runUnify :: [Pair a] -> Either (UnificationError a) (Env a)
  runUnify = runIdentity . runExceptT . unifyMany

  unifiable :: a -> a -> Bool
  unifiable t0 t1 = isRight $ runIdentity $ runExceptT $ unify t0 t1

  (<=>) :: a -> a -> Bool
  (<=>) = unifiable

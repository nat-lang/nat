{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Nat.Evaluation.Core where

import Control.Monad.Cont (Cont, callCC, cont, runCont)
import Control.Monad.Identity (Identity (Identity))
import qualified Data.Set as Set
import Debug.Trace (trace)
import Nat.Context
import qualified Nat.Context as C
import Nat.Reduction
import Nat.Syntax.Core
import Nat.Walk (Walkable (walkC))

data CoreEvalError
  = MaxRecursion Expr
  | UnboundVar Expr

instance Contextual Expr where
  fv :: Expr -> Set.Set Var
  fv = \case
    EVar v -> Set.singleton v
    ELam v e -> fv e Set.\\ Set.singleton v
    EApp e0 e1 -> fv e0 `Set.union` fv e1

instance Substitutable Expr Expr where
  sub' c v e e' = case e' of
    EVar v' | v' == v -> e
    ELam v' b | v' == v -> e'
    e' -> c e'

{-
instance Reducible Expr Expr () CoreEvalError where
  reduce expr = cata $ \case
    -- leftmost, unFixermost
    EApp e0 e1 -> case e0 of
      ELam (Binder v _) e -> sub
-}
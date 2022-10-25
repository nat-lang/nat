{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Nat.Evaluation.Core where

import Control.Monad.Cont (ContT, callCC, cont, runCont)
import Control.Monad.Identity (Identity (Identity))
import qualified Data.Set as Set
import Nat.Cata
import Nat.Context
import qualified Nat.Context as C
import Nat.Reduction
import Nat.Syntax.Core

data CoreEvalError
  = MaxRecursion Expr
  | UnboundVar Expr

instance Contextual AExpr where
  fv' :: AExpr (Set.Set Var) -> Set.Set Var
  fv' = \case
    EVar v -> Set.singleton v
    ELam v e -> e Set.\\ Set.singleton v
    EApp e0 e1 -> e0 `Set.union` e1

instance Substitutable AExpr where
  sub' :: Var -> Expr -> Expr -> Expr
  sub' v e = \case
    In (EVar v') | v' == v -> e
    e' -> e'

alg a v e = \case
  In (EVar v') | v' == v -> e
  In (ELam v' e') | v' == v -> a e'
  e' -> e'

{-
instance Reducible Expr Expr () CoreEvalError where
  reduce expr = cata $ \case
    -- leftmost, outermost
    EApp e0 e1 -> case e0 of
      ELam (Binder v _) e -> sub
-}
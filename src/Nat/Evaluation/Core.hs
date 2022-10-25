{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Nat.Evaluation.Core where

import qualified Data.Set as Set
import Nat.Cata
import Nat.Context (Var)
import Nat.Reduction
import Nat.Syntax.Core

data CoreEvalError
  = MaxRecursion Expr
  | UnboundVar Expr

fv :: Expr -> AExpr (Set.Set Var)
fv = cataM fv'
  where
    fv' (EVar v) = Set.singleton v
    fv' (ELam v e) = fv e Set.\\ Set.singleton v
    fv' (EApp e0 e1) = fv e0 `Set.union` fv e1

{-
instance Substitutable Expr Expr where
  sub v e = cata $ \e' -> case e' of
    EVar v' | v' == v -> e
    ELam (Binder v' t) _ | v' == v -> e'
    _ -> sub v e e'

instance Reducible Expr Expr () CoreEvalError where
  reduce expr = cata $ \case
    -- leftmost, outermost
    EApp e0 e1 -> case e0 of
      ELam (Binder v _) e -> sub
-}
{-# LANGUAGE InstanceSigs #-}

module Nat.Syntax.Core where

import Nat.Cata
import Nat.Context
import Nat.Syntax.Type

data Binder b = Binder b Type

data AExpr a
  = EVar Var
  | ELam Var a
  | EApp a a
  deriving (Eq, Ord, Show)

type Expr = Mu AExpr

instance Functor AExpr where
  fmap :: (a -> b) -> AExpr a -> AExpr b
  fmap f expr = case expr of
    ELam v e -> ELam v (f e)
    EApp e0 e1 -> EApp (f e0) (f e1)
    EVar v -> EVar v

var :: Name -> Expr
var = In . EVar . mkVar

lam :: Var -> Expr -> Expr
lam b = In . ELam b

app :: Expr -> Expr -> Expr
app e = In . EApp e

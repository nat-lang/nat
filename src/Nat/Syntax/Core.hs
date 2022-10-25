{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module Nat.Syntax.Core where

import Data.Traversable
import Nat.Cata
import Nat.Context hiding (var)
import qualified Nat.Context as C
import Nat.Syntax.Type
import Nat.Viz

data Binder b = Binder b Type

data AExpr a
  = EVar Var
  | ELam Var a
  | EApp a a
  deriving (Eq, Ord, Functor, Foldable, Traversable)

type Expr = Mu AExpr

instance Pretty Expr where
  ppr n = cata $ \case
    EVar v -> p v
    ELam v d -> char 'λ' <> p v <> brackets d
    EApp d0 d1 -> d0 <> parens d1

instance Show Expr where
  show :: Expr -> String
  show = show . ppr 0

var :: Name -> Expr
var = In . EVar . C.var

lam :: Var -> Expr -> Expr
lam b = In . ELam b

app :: Expr -> Expr -> Expr
app e = In . EApp e

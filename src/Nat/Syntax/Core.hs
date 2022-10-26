{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Nat.Syntax.Core where

import Control.Arrow ((>>>))
import Data.Eq.Deriving (deriveEq1) -- these two are from the
import Data.Fix
import Data.Traversable
import Nat.Context hiding (var)
import qualified Nat.Context as C
import Nat.Syntax.Type
import Nat.Viz
import Nat.Walk
import Text.Show.Deriving (deriveShow1)

data Binder b = Binder b Type

data AExpr a
  = EVar Var
  | ELam Var a
  | EApp a a
  deriving (Eq, Ord, Functor, Foldable, Traversable)

type Expr = Fix AExpr

instance Pretty a => Pretty (AExpr a) where
  ppr n = \case
    EVar v -> p v
    ELam v e -> char 'λ' <> p v <> brackets (ppr n e)
    EApp e0 e1 -> ppr n e0 <> parens (ppr n e1)

instance Pretty a => Show (AExpr a) where
  show = show . ppr 0

deriveEq1 ''AExpr
deriveShow1 ''AExpr

var :: Name -> Expr
var = Fix . EVar . C.var

lam :: Var -> Expr -> Expr
lam b = Fix . ELam b

app :: Expr -> Expr -> Expr
app e = Fix . EApp e

instance Walkable Expr where
  walkMC' f expr = f expr ctn
    where
      go = walkMC' f
      ctn = \case
        Fix (EApp e0 e1) -> app <$> go e0 <*> go e1
        Fix (ELam b e) -> lam b <$> go e
        Fix e' -> f (Fix e') pure
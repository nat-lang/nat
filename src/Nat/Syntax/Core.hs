{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Nat.Syntax.Core where

import Nat.Context hiding (var)
import qualified Nat.Context as C
import Nat.Syntax.Type
import Nat.Viz
import Nat.Walk

data Binder b = Binder b Type

data Expr
  = EVar Var
  | ELam Var Expr
  | EApp Expr Expr
  deriving (Eq, Ord)

instance Pretty Expr where
  ppr n e = case e of
    EVar v -> p v
    ELam v e ->
      char 'λ' <> p v <> case e of
        ELam {} -> ppr (n + 1) e
        _ -> brackets (ppr (n + 1) e)
    EApp e0 e1 -> ppr n e0 <> parens (ppr n e1)

instance Show Expr where
  show = show . ppr 0

instance Walkable Expr where
  walkMC' f expr = f expr ctn
    where
      go = walkMC' f
      ctn = \case
        EApp e0 e1 -> EApp <$> go e0 <*> go e1
        ELam b e -> ELam b <$> go e
        e' -> f e' return

trav ::
  (Expr -> Expr -> Expr) -> -- constructor factory
  (Expr -> Expr -> Expr) -> -- concatenation
  (Expr -> (Expr -> Expr) -> Expr) -> -- transformation with continuation param
  Expr -> -- base
  Expr
trav cons conc tran expr = cons expr (tran expr next)
  where
    go = trav cons conc tran
    next e' = let cons' = cons e' in case e' of
      EApp e0 e1 -> cons' $ (go e0) `conc` (go e1)
      ELam b e -> cons' (go e)
      e' -> tran e' cons'

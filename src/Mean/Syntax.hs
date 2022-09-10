module Mean.Syntax where

import Mean.Core
import Mean.Case
import Mean.Relations
import Mean.Tree
import Mean.Set

data Expr
  = ELit Lit
  | EVar Var
  | EBind Binder
  | ELam (Lambda Expr)
  | EApp (App Expr)
  | EEq (Eq Expr)
  | ERel (RelExpr Expr)
  | ETree (T.Tree Expr)
  | ECase (Case Expr)
  | ESet (Set Expr)
  -- ESetComp (Expr, Expr)

mkVar :: Name -> Expr
mkVar v = EVar $ mkVar v

mkLam :: Binder -> Expr -> Expr
mkLam b = ELam . Lam b

mkApp :: Expr -> Expr -> Expr
mkApp e = EApp . App e

mkBinder :: Expr -> Binder
mkBinder (EVar v) = Binder v TyNil
mkBinder _ = error "can't bind anything but a variable!"

mkBind :: Expr -> Expr
mkBind = EBind . mkBinder

mkFn :: Expr -> Expr -> Expr
mkFn = mkLam . mkBinder

(*) :: Expr -> Expr -> Expr
(*) = mkApp

(~>) :: Expr -> Expr -> Expr
(~>) = mkFn

infixl 9 *

infixl 8 ~>

true = ELit $ LBool True

false = ELit $ LBool False

(&&) :: Expr -> Expr -> Expr
p && q = EBinOp And p q

(||) :: Expr -> Expr -> Expr
p || q = SBinOp Or p q

eq = SBinOp Eq

(===) = eq

nEq = SBinOp NEq

(!==) = nEq

not' = SUnOp Neg

(?) = STernOp Cond

e > e' = e e'

instance Reducible Expr where
  reduce expr = case expr of
    ELit l -> pure $ CLit l
    EVar var -> pure $ CVar var
    EBind b -> pure $ CBind b
    ELam (Lam b e) -> do
      e' <- reduce e
      pure $ mkCLam b e'
    EApp (App e0 e1) -> do
      e0' <- reduce e0
      e1' <- reduce e1
      pure $ mkCApp (reduce e0') (reduce e1')
    EEq (Eq e0 e1) -> do
      e0' <- reduce e0
      e1' <- reduce e1
      pure $ CEq $ Eq (reduce e0') (reduce e1')
    ETree t -> reduce t
    ECase c -> reduce c
    ESet s -> reduce s
    ERel r -> reduce r
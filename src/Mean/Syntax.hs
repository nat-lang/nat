{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Mean.Syntax where

import Mean.Core
import Mean.Case
import Mean.Relations
import Mean.Tree
import Mean.Set
import Mean.Viz
import Control.Monad.State
import qualified Mean.Parser as P

import Prelude hiding (Eq)

data Expr
  = ELit Lit
  | EVar Var
  | EBind Binder
  | ELam (Lambda Expr)
  | EApp (App Expr)
  | EEq (Eq Expr)
  | ERel RelExpr
  | ETree (Tree Expr)
  | ECase CaseExpr
  | ESet SetExpr

instance Pretty (Lambda Expr) where
  ppr p (Lam b e) =
    ppr p b <> case e of
      ELam Lam {} -> ppr (p + 1) e
      _ -> brackets (ppr (p + 1) e)

instance Pretty Expr where
  ppr p e = case e of
    ELit l -> ppr p l
    EVar v -> text $ show v
    EBind b -> ppr p b
    ELam lam -> ppr p lam
    EApp app -> ppr p app
    EEq eq -> ppr p eq
    ERel rel -> ppr p rel
    ETree t -> ppr p t
    ECase c -> ppr p c
    ESet s -> ppr p s

instance Show Expr where
  show = show . ppr 0

{-
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
-}

instance Reducible Expr where
  reduce expr = case expr of
    ELit l -> pure $ CLit l
    EVar var -> pure $ CVar var
    EBind b -> pure $ CBind b
    ELam (Lam b e) -> do
      e' <- reduce e
      reduce $ mkCLam b e'
    EApp (App e0 e1) -> do
      e0' <- reduce e0
      e1' <- reduce e1
      reduce $ mkCApp e0' e1'
    EEq (Eq e0 e1) -> do
      e0' <- reduce e0
      e1' <- reduce e1
      reduce $ CEq $ Eq e0' e1'
    ETree t -> reduce t
    ECase c -> reduce c
    ESet s -> reduce s
    ERel r -> reduce r

-------------------------------------------------------------------------------
-- Parsing
-------------------------------------------------------------------------------

type ExprParser = P.Parser Expr

pELam :: ExprParser
pELam = do
  lam <- pLam
  ELam . lam <$> pExpr

finally fn p = do
  mA <- P.observing p
  case mA of
    Left e -> fn >> P.parseError e
    Right a -> fn >> pure a

pETree :: ExprParser
pETree = do
  s <- get
  put (s {P.inTree = True})
  t <- finally (put $ s {P.inTree = False}) (pTree pExpr)
  pure $ ETree t

terms =
  [ P.parens pExpr,
    ELit <$> pLit,
    EVar <$> pVar,
    EBind <$> pBinder,
    pELam,
    ECase <$> pCase pExpr,
    ESet <$> pSet pExpr,
    pRel pExpr ERel
  ]

pTerm :: ExprParser
pTerm = do
  s <- get
  P.choice $
    [pETree | not (P.inTree s)] ++ terms

operatorTable :: [[P.Operator P.Parser Expr]]
operatorTable =
  [[ P.infixOpL "==" (\e0 e1 -> EEq (Eq e0 e1)) ]]

pExpr' :: ExprParser
pExpr' = P.makeExprParser pTerm operatorTable

pExpr :: ExprParser
pExpr = do
  exprs <- P.some pExpr'
  pure (foldl1 (\e0 e1 -> EApp $ App e0 e1) exprs)

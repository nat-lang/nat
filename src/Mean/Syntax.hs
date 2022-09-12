{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Mean.Syntax where

import Mean.Core hiding (mkBinder, mkFn, eq)
import Mean.Case
import Mean.Relations hiding ((?), (>), nEq, not', (&&), (||))
import Mean.Tree
import Mean.Set
import Mean.Viz
import Control.Monad.State
import qualified Mean.Parser as P

import Prelude hiding (Eq)
import qualified Prelude as Prel

data Expr
  = ELit Lit
  | EVar Var
  | EBind Binder
  | ELam (Lambda Expr)
  | EApp (App Expr)
  | EEq (Eq Expr)
  | ECond (Cond Expr)
  | ERel (RelExpr Expr)
  | ETree (Tree Expr)
  | ECase (CaseExpr Expr)
  | ESet (SetExpr Expr)
  deriving (Prel.Eq)

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

mkEVar :: Name -> Expr
mkEVar = EVar . mkVar

mkELam :: Binder -> Expr -> Expr
mkELam b = ELam . Lam b

mkEApp :: Expr -> Expr -> Expr
mkEApp e = EApp . App e

mkBinder :: Expr -> Binder
mkBinder (EVar v) = Binder v TyNil
mkBinder _ = error "can't bind anything but a variable!"

mkEBind :: Expr -> Expr
mkEBind = EBind . mkBinder

mkEFn :: Expr -> Expr -> Expr
mkEFn = mkELam . mkBinder

(*) :: Expr -> Expr -> Expr
(*) = mkEApp

(~>) :: Expr -> Expr -> Expr
(~>) = mkEFn

infixl 9 *

infixl 8 ~>

f :: Expr
f = mkEVar "f"

x :: Expr
x = mkEVar "x"

y :: Expr
y = mkEVar "y"

z :: Expr
z = mkEVar "z"

true = ELit $ LBool True

false = ELit $ LBool False

(&&) :: Expr -> Expr -> Expr
p && q = ERel (RBinOp And p q)

(||) :: Expr -> Expr -> Expr
p || q = ERel (RBinOp Or p q)

eq e0 e1 = EEq (Eq e0 e1)

(===) = eq

nEq :: Expr -> Expr -> Expr
nEq e0 e1 = ERel (RBinOp NEq e0 e1)

(!==) :: Expr -> Expr -> Expr
(!==) = nEq

not' :: Expr -> Expr
not' = ERel . RUnOp Neg

(?) x y z = ECond (Cond x y z)

e > e' = e e'

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
    ECond (Cond x y z) -> do
      x' <- reduce x
      y' <- reduce y
      z' <- reduce z
      reduce $ CCond $ Cond x' y' z'
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

after p fn = do
  mA <- P.observing p
  case mA of
    Left e -> fn >> P.parseError e
    Right a -> fn >> pure a

pETree :: ExprParser
pETree = do
  s <- get
  put (s {P.inTree = True})
  t <- after (pTree pENode) (put $ s {P.inTree = False})
  pure $ ETree t
  where
    pEBind = EBind <$> pBinder
    pENode = P.try pExpr P.<|> pEBind

pECase =  ECase <$> pCase pExpr
pESet = ESet <$> pSet pExpr

terms =
  [ P.parens pExpr,
    ELit <$> pLit,
    EVar <$> pVar,
    ECond <$> pCond pExpr,
    pELam,
    pECase,
    pESet
  ]

pTerm :: ExprParser
pTerm = do
  s <- get
  P.choice $
    [pETree | not (P.inTree s)] ++ terms

operatorTable :: [[P.Operator P.Parser Expr]]
operatorTable =
  [
    [ P.prefixOp "!" (ERel . RUnOp Neg) ],
    [ P.infixOpL "==" (\e0 e1 -> EEq (Eq e0 e1)),
      P.infixOpL "!=" (biExpr NEq),
      P.infixOpL "&&" (biExpr And),
      P.infixOpL "||" (biExpr Or)
    ]
  ] where
    biExpr = \op e0 e1 -> ERel (RBinOp op e0 e1)

pExpr' :: ExprParser
pExpr' = P.makeExprParser pTerm operatorTable

pExpr :: ExprParser
pExpr = do
  exprs <- P.some pExpr'
  pure (foldl1 (\e0 e1 -> EApp $ App e0 e1) exprs)

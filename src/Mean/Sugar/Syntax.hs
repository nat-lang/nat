module Mean.Sugar.Syntax
  ( SugarExpr (..),
    T.Tree (..),
    ExprNode (..),
    ExprTree,
    mkSVar,
    mkSLam,
    mkSApp,
  )
where

import qualified Data.Tree.Binary.Preorder as T
import Mean.Core.Syntax

data ExprNode = ENode CoreExpr | BNode Binder deriving (Eq)

type ExprTree = T.Tree ExprNode

-- data Relation -- n-place fn with base ty == tyBool
-- data Set -- 1-place relation
-- data Predicate -- 1-place relation
-- data Quant -- 2-place relation

data SugarExpr
  = SLit Lit
  | SVar Var
  | STree ExprTree
  | SLam (Lambda SugarExpr)
  | SApp (App SugarExpr)
  deriving (Eq)

-- SPatternMatch Core.Var
-- SRelation Core.Lambda
-- Atom Core.Var
-- Domain Core.Lambda
-- n-place fn
-- ApplicativeFunctor
-- Monad

mkSVar :: Name -> SugarExpr
mkSVar v = SVar $ mkVar v

mkSLam :: Binder -> SugarExpr -> SugarExpr
mkSLam b = SLam . Lam b

mkSApp :: SugarExpr -> SugarExpr -> SugarExpr
mkSApp e = SApp . App e

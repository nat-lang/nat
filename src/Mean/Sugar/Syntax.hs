module Mean.Sugar.Syntax
  ( T.Tree (..),
    T.drawTree,
    module Mean.Sugar.Syntax,
  )
where

import qualified Data.Tree.Binary.Preorder as T
import Mean.Common.Viz
import Mean.Core.Syntax hiding (mkBinder, mkFn, (*), (~>))
import Mean.Core.Viz
import Text.PrettyPrint
import Prelude hiding ((<>))

type ExprTree = T.Tree SugarExpr

-- n-place fn with base ty == tyBool
newtype Relation = Rel (Lambda SugarExpr) deriving (Eq)

data SugarExpr
  = -- core exprs paramaterized for sugar
    SLit Lit
  | SVar Var
  | SBind Binder
  | SLam (Lambda SugarExpr)
  | SApp (App SugarExpr)
  | SUnOp UnOp SugarExpr
  | SBinOp BinOp SugarExpr SugarExpr
  | STernOp TernOp SugarExpr SugarExpr SugarExpr
  | -- sugar
    STree ExprTree
  | SCase SugarExpr [(SugarExpr, SugarExpr)]
  | SSet [SugarExpr]
  | SSetComp (SugarExpr, SugarExpr)
  | SRel Relation
  deriving (Eq)

-- data Set -- 1-place relation
-- data Predicate -- 1-place relation
-- logical operators -- 2-place relations
-- quantifiers -- 2-place relations between 1-place relations

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

mkBinder :: SugarExpr -> Binder
mkBinder (SVar v) = Binder v TyNil
mkBinder _ = error "can't bind anything but a variable!"

mkSBind :: SugarExpr -> SugarExpr
mkSBind = SBind . mkBinder

mkFn :: SugarExpr -> SugarExpr -> SugarExpr
mkFn = mkSLam . mkBinder

(*) :: SugarExpr -> SugarExpr -> SugarExpr
(*) = mkSApp

(~>) :: SugarExpr -> SugarExpr -> SugarExpr
(~>) = mkFn

infixl 9 *

infixl 8 ~>
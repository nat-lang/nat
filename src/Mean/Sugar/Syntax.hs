{-# LANGUAGE FlexibleInstances #-}
module Mean.Sugar.Syntax
  ( T.Tree (..),
    T.drawTree,
    module Mean.Sugar.Syntax,
  )
where

import qualified Data.Tree.Binary.Preorder as T
import Mean.Core.Syntax hiding (mkBinder, mkFn, (*), (~>))
import Mean.Common.Viz
import Mean.Core.Viz
import Text.PrettyPrint
import Prelude hiding ((<>))

instance Pretty (Lambda SugarExpr) where
  ppr p (Lam (Binder n t) e) =
    char 'λ' <> text (show n) <> case e of
      SLam Lam {} -> ppr (p + 1) e
      _ -> brackets (ppr (p + 1) e)

instance Pretty SugarExpr where
  ppr p e = case e of
    SLit l -> ppr p l
    SVar v -> text $ show v
    SBind b -> ppr p b
    SLam l -> ppr p l
    SApp a -> ppr p a
    STree t -> text $ T.drawTree t

instance Show SugarExpr where
  show = show . ppr 0

type ExprTree = T.Tree SugarExpr

-- data Relation -- n-place fn with base ty == tyBool
-- data Set -- 1-place relation
-- data Predicate -- 1-place relation
-- data Quant -- 2-place relation

data SugarExpr
  = SLit Lit
  | SVar Var
  | STree ExprTree
  | SBind Binder
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

mkBinder :: SugarExpr -> Binder
mkBinder (SVar v) = Binder v TyNil
mkBinder e = error ("can't bind anything but a variable! " ++ show e)

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
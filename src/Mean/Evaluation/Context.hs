module Mean.Evaluation.Context where

import Data.Foldable (toList)
import qualified Data.Set as Set
import Mean.Context
import Mean.Syntax.Surface
import Mean.Unification

instance Contextual Expr where
  fv expr = case expr of
    EVar v -> Set.singleton v
    ELam (Binder v _) body -> fv body Set.\\ Set.singleton v
    EApp e0 e1 -> fv [e0, e1]
    ECond x y z -> fv [x, y, z]
    EUnOp _ e -> fv e
    EBinOp _ e0 e1 -> fv [e0, e1]
    ETree t -> fv (toList t)
    ETup es -> fv es
    -- ELitCase Expr [(Expr, Expr)]
    -- ESet (Set Expr)
    -- ELet Var Expr Expr
    -- EFix Var Expr
    _ -> Set.empty

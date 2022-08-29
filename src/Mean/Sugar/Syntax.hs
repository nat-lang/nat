module Mean.Sugar.Syntax
  ( Expr (..),
    T.Tree (..),
    ExprTree,
  )
where

import qualified Mean.Core.Syntax as Core
import qualified Data.Tree.Binary.Preorder as T

type ExprTree = T.Tree Core.Expr

-- data Op
-- data Quant -- binding structure
-- data Relation -- n-place fn with base ty = tyBool
-- data Set -- 1-place relation
-- data Predicate -- 1-place relation

data Expr
  = ECore Core.Expr
  | ETree ExprTree
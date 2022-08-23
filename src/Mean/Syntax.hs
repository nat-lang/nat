module Mean.Syntax
  ( Expr (..),
    Binder (..),
    Lit (..),
    T.Tree (..),
    T.printTree,
    ExprTree,
  )
where

import qualified Data.Tree.Binary.Preorder as T
import GHC.Generics
import Mean.Typing (Type)

type Name = String

type ExprTree = T.Tree Expr

data Lit
  = LInt Int
  | LBool Bool
  deriving (Eq, Ord)

data Binder = Binder Name Type deriving (Eq, Ord)

data Expr
  = ELit Lit
  | Var Name
  | Lam Binder Expr
  | App Expr Expr
  | Tree ExprTree
  deriving (Eq, Ord)
module Mean.Syntax
  ( Name,
    ExprTree,
    TyVar (..),
    Type (..),
    TyScheme (..),
    Binder (..),
    Expr (..),
    Lit (..),
    T.Tree (..),
    tyInt,
    tyBool,
  )
where

import qualified Data.Tree.Binary.Preorder as T
import GHC.Generics

newtype TyVar = TV String
  deriving (Show, Eq, Ord)

data Type
  = TyVar TyVar
  | TyCon String
  | TyFun Type Type
  | TyNil
  deriving (Eq, Ord)

data TyScheme = Forall [TyVar] Type
  deriving (Eq, Ord)

tyInt, tyBool :: Type
tyInt = TyCon "n"
tyBool = TyCon "t"

type Name = String

type ExprTree = T.Tree Expr

data Lit
  = LInt Int
  | LBool Bool
  deriving (Eq, Ord, Show)

data Binder = Binder Name Type deriving (Eq, Ord)

data Expr
  = ELit Lit
  | Var Name
  | Lam Binder Expr
  | App Expr Expr
  | Tree ExprTree
  deriving (Eq, Ord)
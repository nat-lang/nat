module Mean.Syntax
  ( Name,
    Var (..),
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
    mkVar,
    mkEVar,
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

data Var = Var Name Name

instance Eq Var where
  (Var _ v) == (Var _ v') = v == v'

instance Ord Var where
  compare (Var _ v0) (Var _ v1) = compare v0 v1

instance Show Var where
  show (Var vPub vPri) = vPri

data Binder = Binder Var Type deriving (Eq, Ord)

data Expr
  = ELit Lit
  | EVar Var
  | Lam Binder Expr
  | App Expr Expr
  | Tree ExprTree
  | Let Name Expr
  deriving (Eq)

mkVar v = Var v (v ++ "0")

mkEVar v = EVar $ mkVar v
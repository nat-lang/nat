module Mean.Core.Syntax where

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

mkUnqScheme :: Type -> TyScheme
mkUnqScheme = Forall []

mkTv :: String -> Type
mkTv = TyVar . TV

tyInt, tyBool :: Type
tyInt = TyCon "n"
tyBool = TyCon "t"

type Name = String

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
  show (Var vPub vPri) = vPub

data Binder = Binder Var Type deriving (Eq, Ord)

data Lambda a = Lam Binder a deriving (Eq)

data App a = App a a deriving (Eq)

data UnOp = Neg deriving (Eq)

data BinOp = Eq | NEq | And | Or deriving (Eq)

data TernOp = Cond deriving (Eq)

data CoreExpr
  = CLit Lit
  | CVar Var
  | CBind Binder
  | CLam (Lambda CoreExpr)
  | CApp (App CoreExpr)
  | CUnOp UnOp CoreExpr
  | CBinOp BinOp CoreExpr CoreExpr
  | CTernOp TernOp CoreExpr CoreExpr CoreExpr
  deriving (Eq)

mkCBool :: Bool -> CoreExpr
mkCBool = CLit . LBool

mkVar :: Name -> Var
mkVar v = Var v (v ++ "0")

mkCVar :: Name -> CoreExpr
mkCVar v = CVar $ mkVar v

mkCLam :: Binder -> CoreExpr -> CoreExpr
mkCLam b = CLam . Lam b

mkCApp :: CoreExpr -> CoreExpr -> CoreExpr
mkCApp e = CApp . App e

mkBinder :: CoreExpr -> Binder
mkBinder (CVar v) = Binder v TyNil
mkBinder _ = error "can't bind anything but a variable!"

mkCBind :: CoreExpr -> CoreExpr
mkCBind = CBind . mkBinder

mkFn :: CoreExpr -> CoreExpr -> CoreExpr
mkFn = mkCLam . mkBinder

(*) :: CoreExpr -> CoreExpr -> CoreExpr
(*) = mkCApp

(~>) :: CoreExpr -> CoreExpr -> CoreExpr
(~>) = mkFn

infixl 9 *

infixl 8 ~>
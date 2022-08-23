module Mean.Syntax
  ( Expr (..),
    Lit (..)
  )
where

import Mean.Typing (Type)

type Name = String

data Lit
  = LInt Int
  | LBool Bool
  | LString String
  deriving (Show, Eq, Ord)

data Expr
  = ELit Lit
  | Var Name
  | Lam Name Type Expr
  | App Expr Expr
  deriving (Eq, Ord)
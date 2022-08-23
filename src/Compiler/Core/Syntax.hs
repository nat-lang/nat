{-# LANGUAGE PatternSynonyms #-}

module Compiler.Core.Syntax
  ( Name,
    Lit (..),
    Binder (..),
    Expr (..),
    Quant (..),
    UnOp (..),
    BinOp (..),
    Comparison (..),
    Type (..),
    TyVar (..),
    TyScheme (..),
    Decl (..),
    rename,
    substitute,
    resolvePredicates,
    tyInt,
    tyBool,
    pattern TyIntP,
    pattern TyBoolP,
  )
where

type Name = String

data Lit
  = LInt Int
  | LBool Bool
  deriving (Show, Eq, Ord)

data Binder = Binder Name Type deriving (Eq, Ord)

data Expr
  = ELit Lit
  | Var Name
  | Const Name Type
  | Lam Binder Expr
  | App Expr Expr
  | EBinder Binder
  | EBinOp BinOp Expr Expr
  | EUnOp UnOp Expr
  | EComparison Comparison Expr Expr
  | Pred Name Type [Expr]
  | EQuant Quant Binder Expr
  deriving (Eq, Ord)

data Quant = Univ | Exis | Iota deriving (Eq, Ord)

data Comparison
  = Eq
  | LT
  | GT
  | SetSubS
  | SetMem
  deriving (Eq, Ord)

data UnOp
  = Neg
  | SetCompl
  deriving (Eq, Ord)

data BinOp
  = Conj
  | Disj
  | Impl
  | Add
  | Mul
  | Sub
  | Div
  | SetUnion
  | SetInter
  | SetDiff
  deriving (Eq, Ord)

newtype TyVar = TV String
  deriving (Show, Eq, Ord)

data Type
  = TyVar TyVar
  | TyCon String
  | TyFun Type Type
  | TyNil
  deriving (Eq, Ord)

tyInt, tyBool :: Type
tyInt = TyCon "n"
tyBool = TyCon "t"

pattern TyIntP = TyCon "n"

pattern TyBoolP = TyCon "t"

data TyScheme = Forall [TyVar] Type
  deriving (Eq, Ord)

data Decl = Let Name Expr | Typedef Name Type

-- rename n to n' in e
rename :: Name -> Name -> Expr -> Expr
rename n n' e = case e of
  Var v | n == v -> Var n'
  Const c t | n == c -> Const n' t
  Pred name t args -> Pred (if n == name then n' else name) t (map rn args)
  EUnOp op e' -> EUnOp op (rn e')
  EBinOp op e0 e1 -> EBinOp op (rn e0) (rn e1)
  EComparison c e0 e1 -> EComparison c (rn e0) (rn e1)
  Lam b e' -> Lam b (rn e')
  EQuant q b e' -> EQuant q b (rn e')
  App e0 e1 -> App (rn e0) (rn e1)
  _ -> e
  where
    rn = rename n n'

-- sub a for every match(n) in e
substitute' :: Expr -> (Name -> Bool) -> Expr -> Expr
substitute' a match e = case e of
  Var n | match n -> a
  Pred name t args -> Pred name t (map sub args)
  EUnOp op e' -> EUnOp op (sub e')
  EBinOp op e0 e1 -> EBinOp op (sub e0) (sub e1)
  EComparison c e0 e1 -> EComparison c (sub e0) (sub e1)
  Lam b@(Binder n _) body -> Lam b (substitute' a (\n' -> match n' && n' /= n) body)
  EQuant q b@(Binder n _) e' -> EQuant q b (substitute' a (\n' -> match n' && n' /= n) e')
  App e0 e1 -> App (sub e0) (sub e1)
  _ -> e
  where
    sub = substitute' a match

substitute :: Expr -> Name -> Expr -> Expr
substitute a n e = substitute' a (\n' -> n == n') e

-- | Construct Syn.Pred expressions out of applications of Syn.Const
--   to arbitrary expressions. To revisit: why can't this be parsed initially?
--
--   (a) Predicates are typed as functions (what else are they?) but
--   (b) they are not evaluated (i.e not beta reduced) like functions.
--   (c) They can be passed around as variables before application.
--
--   At least one complication is that initial resolution won't apply
--   to predicates that fall under (c), so even if predicates are parsed
--   as such initially, this resolution step will have to be done after
--   each beta reduction. Using only this function isolates the logic.
--
--   6/20 predicates should be identified by the parser as typed constants,
--   and the application step of evaluation should return a VPred value
resolvePredicates :: Expr -> Expr
resolvePredicates = go
  where
    go expr = case expr of
      App e0 e1 ->
        let r1 = go e1
         in case e0 of
              Const c t -> Pred c t [r1]
              _ ->
                let r0 = go e0
                 in case r0 of
                      Pred n t args -> Pred n t (args ++ [r1])
                      _ -> App r0 r1
      Lam b e -> Lam b (go e)
      EQuant q b e -> EQuant q b (go e)
      EBinOp op e0 e1 -> EBinOp op (go e0) (go e1)
      EUnOp op e -> EUnOp op (go e)
      _ -> expr

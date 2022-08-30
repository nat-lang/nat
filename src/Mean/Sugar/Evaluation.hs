{-# LANGUAGE PatternSynonyms #-}

module Mean.Sugar.Evaluation where

import qualified Mean.Core.Evaluation as CEval
import qualified Mean.Core.Syntax as CSyn
import Mean.Core.Type
import Mean.Sugar.Syntax

{-
data EvalError = NotAFn SugarExpr SugarExpr deriving (Eq)

type Evaluation = ExceptT EvalError Identity

type EvaluationM = CEval.Evaluation Expr

type ENode = (CSyn.Type, CSyn.Expr)

pattern FnNode e tDom <- TypedExpr e (S.TyFun tDom _)

pattern ArgNode e t <- TypedExpr e t

unify (t0, e0) (t1, e1) = case (t0 <=> t1) of
  True -> Just (e0, e1)
  False -> case (t1 <=> t0) of
    True -> Just (e0, e1)
    False -> Nothing

-- λlλr .
functionApplication :: ENode -> ENode -> EvaluationM
functionApplication n0 n1 = case unify n0 n1 of
  Just (e0, e1) -> S.App fn arg
  Nothing -> if t0 <=> t1
    then
    else if

  case ((t0, e0), (t1, e1)) of

  <=>

  (FnNode fn tDom, TypedExpr arg t) | Inf.unifiable tDom t -> doFA fn arg
  (TypedExpr arg t, FnNode fn tDom) | Inf.unifiable tDom t -> doFA fn arg
  _ -> Nothing
  where
    doFA fn arg = Just $

pattern BinderNode b <- TypedExpr (S.EBinder b) _

predicateAbstraction :: ENode -> ENode -> CEvaluationM
predicateAbstraction e0 e1 = case (e0, e1) of
  (BinderNode b, TypedExpr e t) -> doPA b e
  (TypedExpr e t, BinderNode b) -> doPA b e
  _ -> Nothing
  where
    doPA b e = Just $ S.Lam b e

predicateModification :: ENode -> ENode -> CEvaluationM
predicateModification e0 e1 = case (e0, e1) of
  (TypedExpr e t, TypedExpr e' t') | Inf.unifiable t t' -> doPM e e'
  _ -> Nothing
  where
    doPM e e' = Just $ S.EBinOp S.Conj e e'

reduce :: Expr -> EvaluationM
reduce expr = case expr of
  SCore e -> pure $ SCore (CEval.eval e)
  STree t -> case t of
    Node e l f -> pure $ Node e' l' f' where
      l' = reduce l
      r' = reduce r
      e' =
    Leaf -> ETree Leaf
-}

reduce' :: Expr -> EvaluationM
reduce' expr = case expr of
  SApp e0 e1 -> case e0 of
    STree {} -> throwError $ NotAFn e0 e1
    SLam {} -> case e1 of

  STree t -> case t of
    Node e l f -> pure $ Node e' l' f' where
      l' = reduce l
      r' = reduce r
      e' =
    Leaf -> ETree Leaf

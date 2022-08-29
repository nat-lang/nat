{-# LANGUAGE FlexibleContexts #-}

module Mean.Evaluation where

import Control.Monad.Except
import Control.Monad.Identity
import Data.Char
import Data.Functor ((<&>))
import qualified Data.Map as Map
import Data.Set ((\\))
import qualified Data.Set as Set
import Debug.Trace (trace, traceM)
import Mean.Err
import qualified Mean.Syntax as S

data EvalError = UnboundVariable S.Name | NotAFn S.Expr S.Expr deriving (Eq)

type Evaluation = ExceptT EvalError Identity

fv :: S.Expr -> Set.Set S.Name
fv e = case e of
  S.Lam (S.Binder (S.Var _ v) _) body -> fv body \\ Set.singleton v
  S.App e0 e1 -> fv e0 `Set.union` fv e1
  S.EVar (S.Var _ v) -> Set.singleton v
  _ -> Set.empty

fresh :: S.Var -> Set.Set S.Name -> S.Var -> Set.Set S.Name -> S.Var
fresh v0 fv0 v1 fv1 =
  let v1'@(S.Var _ v1'Pri) = incr v1
   in if v1' == v0 || Set.member v1'Pri fv0 || Set.member v1'Pri fv1
        then fresh v0 fv0 v1' fv1
        else v1'
  where
    incr (S.Var vPub vPri) = S.Var vPub $ init vPri ++ show (digitToInt (last vPri) + 1)

-- e'[e/v]
sub :: S.Expr -> S.Var -> S.Expr -> S.Expr
sub e v e' = case e' of
  -- relevant base case
  S.EVar v'@S.Var {} | v' == v -> e
  -- induction
  S.App e0 e1 -> S.App (sub e v e0) (sub e v e1)
  -- induction, but rename binder if it conflicts with fv(e)
  S.Lam b@(S.Binder v'@(S.Var _ v'Pri) t) body
    | v /= v' ->
      let fv0 = fv e
          fv1 = fv body
       in if v'Pri `Set.member` fv0
            then
              let v'' = fresh v fv0 v' fv1
                  body' = sub e v $ sub (S.EVar v'') v' body
               in S.Lam (S.Binder v'' t) body'
            else S.Lam b (sub e v body)
  -- irrelevent base cases
  _ -> e'

--  Normal order reduction.
reduce :: S.Expr -> Evaluation S.Expr
reduce expr = case expr of
  S.App e0 e1 -> do
    case e0 of
      -- (1a) leftmost, outermost
      S.App {} -> do
        e0' <- reduce e0
        case e0' of
          -- (1a.1) normal form for lhs (app), goto rhs
          S.App {} -> do
            e1' <- reduce e1
            pure (S.App e0' e1')
          -- otherwise goto top
          _ -> reduce (S.App e0' e1)
      -- (1b) normal form for lhs (free var), goto rhs
      S.EVar {} -> do
        e1' <- reduce e1
        pure (S.App e0 e1')
      -- (1c) function app, beta reduce
      S.Lam (S.Binder v _) body -> do
        reduce (sub e1 v body)
      S.ELit {} -> pure expr
  -- (2) simplify lambda body
  S.Lam b body -> do
    body' <- reduce body
    pure $ S.Lam b body'
  -- (3) var/literal
  _ -> pure expr

-- assumes e0 and e1 are in normal form
alphaEq :: S.Expr -> S.Expr -> Bool
alphaEq e0 e1 = case (e0, e1) of
  (l0@(S.Lam (S.Binder v0 _) body0), l1@(S.Lam (S.Binder v1 _) body1)) ->
    sub (S.EVar v1) v0 body0 @= body1
      || sub (S.EVar v0) v1 body1 @= body0
  (S.App e0a e0b, S.App e1a e1b) -> e0a @= e1a && e0b @= e1b
  (S.EVar v0, S.EVar v1) -> v0 == v1
  (S.ELit l0, S.ELit l1) -> l0 == l1
  -- to be reconsidered
  (S.Let v0 e0, S.Let v1 e1) -> v0 == v1 && e0 @= e1
  _ -> False

e0 @= e1 = alphaEq e0 e1

eval :: S.Expr -> Either EvalError S.Expr
eval e = runIdentity $ runExceptT $ reduce e

confluent e0 e1 = case (eval e0, eval e1) of
  (Right e1', Right e0') -> e0' @= e1'
  _ -> False

e0 *= e1 = confluent e0 e1
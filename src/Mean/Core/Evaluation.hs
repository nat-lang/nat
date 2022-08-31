{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}

module Mean.Core.Evaluation where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Identity (Identity (runIdentity))
import Data.Char (digitToInt)
import Data.Functor ((<&>))
import qualified Data.Map as Map
import Data.Set ((\\))
import qualified Data.Set as Set
import Mean.Core.Patterns (pattern App, pattern Lam)
import qualified Mean.Core.Syntax as S

data EvalError
  = UnboundVariable S.Name
  | NotAFn S.CoreExpr S.CoreExpr
  | NotAnArg S.CoreExpr S.CoreExpr
  deriving (Eq)

type Evaluation = ExceptT EvalError Identity

fv :: S.CoreExpr -> Set.Set S.Name
fv e = case e of
  Lam (S.Binder (S.Var _ v) _) body -> fv body \\ Set.singleton v
  App e0 e1 -> fv e0 `Set.union` fv e1
  S.CVar (S.Var _ v) -> Set.singleton v
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
sub :: S.CoreExpr -> S.CoreExpr -> S.CoreExpr -> S.CoreExpr
sub e cv@(S.CVar v) e' = case e' of
  -- relevant base case
  S.CVar v' | v' == v -> e
  -- induction
  App e0 e1 -> S.mkCApp (sub e cv e0) (sub e cv e1)
  -- induction, but rename binder if it conflicts with fv(e)
  Lam b@(S.Binder v'@(S.Var _ v'Pri) t) body
    | v /= v' ->
      let fv0 = fv e
          fv1 = fv body
       in if v'Pri `Set.member` fv0
            then
              let v'' = fresh v fv0 v' fv1
                  body' = sub e cv $ sub (S.CVar v'') (S.CVar v') body
               in S.mkCLam (S.Binder v'' t) body'
            else S.mkCLam b (sub e cv body)
  -- irrelevent base cases
  _ -> e'
sub _ _ _ = error "can't substitute for anything but a variable!"

--  Normal order reduction.
reduce :: S.CoreExpr -> Evaluation S.CoreExpr
reduce expr = case expr of
  App e0 e1 -> case e1 of
    S.CBind b -> throwError $ NotAnArg e1 e0
    _ -> case e0 of
      -- (1a) leftmost, outermost
      App {} -> do
        e0' <- reduce e0
        case e0' of
          -- (1a.1) normal form for lhs (app), goto rhs
          App {} -> do
            e1' <- reduce e1
            pure (S.mkCApp e0' e1')
          -- (1a.2) otherwise goto top
          _ -> reduce (S.mkCApp e0' e1)
      -- (1b) normal form for lhs (free var), goto rhs
      S.CVar {} -> do
        e1' <- reduce e1
        pure (S.mkCApp e0 e1')
      -- (1c) binders have a semantics of their own: they may be applied
      -- to terms, in which case they simply abstract a free variable.
      S.CBind b -> reduce (S.mkCLam b e1)
      -- (1d) function application, beta reduce
      Lam (S.Binder v _) body -> reduce (sub e1 (S.CVar v) body)
      S.CLit {} -> pure expr
  -- (2) simplify lambda body
  Lam b body -> do
    body' <- reduce body
    pure $ S.mkCLam b body'
  -- (3) var/literal
  _ -> pure expr

-- assumes e0 and e1 are in normal form
alphaEq :: S.CoreExpr -> S.CoreExpr -> Bool
alphaEq e0 e1 = case (e0, e1) of
  (Lam (S.Binder v0 _) body0, Lam (S.Binder v1 _) body1) -> let
      v0' = S.CVar v0
      v1' = S.CVar v1
    in
      sub v1' v0' body0 @= body1 || sub v0' v1' body1 @= body0
  (App e0a e0b, App e1a e1b) -> e0a @= e1a && e0b @= e1b
  (S.CVar v0, S.CVar v1) -> v0 == v1
  (S.CLit l0, S.CLit l1) -> l0 == l1
  _ -> False

(@=) :: S.CoreExpr -> S.CoreExpr -> Bool
e0 @= e1 = alphaEq e0 e1

eval :: S.CoreExpr -> Either EvalError S.CoreExpr
eval e = runIdentity $ runExceptT $ reduce e

confluent :: S.CoreExpr -> S.CoreExpr -> Bool
confluent e0 e1 = case (eval e0, eval e1) of
  (Right e1', Right e0') -> e0' @= e1'
  _ -> False

(*=) :: S.CoreExpr -> S.CoreExpr -> Bool
e0 *= e1 = confluent e0 e1
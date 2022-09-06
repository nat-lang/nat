{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}

module Mean.Core.Evaluation where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Identity (Identity (runIdentity))
import Data.Char (digitToInt)
import qualified Data.Map as Map
import Data.Set ((\\))
import qualified Data.Set as Set
import Debug.Trace (traceM)
import Mean.Core.Patterns (pattern App, pattern CFalse, pattern CTrue, pattern Lam)
import qualified Mean.Core.Syntax as S
import Mean.Core.Viz

data EvalError
  = UnboundVariable S.Name
  | NotAFn S.CoreExpr S.CoreExpr
  | NotTruthy S.CoreExpr
  deriving (Eq)

instance Show EvalError where
  show (UnboundVariable n) = "Unbound variable: " ++ show n
  show (NotAFn e0 e1) = "Can't apply " ++ show e0 ++ " to " ++ show e1
  show (NotTruthy e) = "Not a truthy expression: " ++ show e

type Evaluation = ExceptT EvalError Identity

class FV a where
  fv :: a -> Set.Set S.Name

instance FV S.CoreExpr where
  fv e = case e of
    S.CVar (S.Var _ v) -> Set.singleton v
    Lam (S.Binder (S.Var _ v) _) body -> fv body \\ Set.singleton v
    App e0 e1 -> fv [e0, e1]
    S.CUnOp _ e -> fv e
    S.CBinOp _ e0 e1 -> fv [e0, e1]
    S.CTernOp _ x y z -> fv [x, y, z]
    _ -> Set.empty

instance FV [S.CoreExpr] where
  fv es = foldl1 Set.union (fv <$> es)

bool :: S.CoreExpr -> Bool
bool e = case e of
  (S.CLit (S.LBool b)) -> b
  _ -> error "can only cast booleans"

incrVarId :: S.Var -> S.Var
incrVarId (S.Var vPub vPri) = S.Var vPub $ init vPri ++ show (digitToInt (last vPri) + 1)

fresh :: S.Var -> S.Var -> Set.Set S.Name -> S.Var
fresh v0 v1 fv =
  let v0'@(S.Var _ v0'Pri) = incrVarId v0
   in if v0' == v1 || Set.member v0'Pri fv
        then fresh v0' v1 fv
        else v0'

-- e'[e/v]
sub :: S.CoreExpr -> S.CoreExpr -> S.CoreExpr -> S.CoreExpr
sub e cv@(S.CVar v) e' =
  let sub' = sub e cv
   in case e' of
        -- relevant base case
        S.CVar v' | v' == v -> e
        -- induction
        App e0 e1 -> S.mkCApp (sub' e0) (sub' e1)
        S.CBinOp op e0 e1 -> S.CBinOp op (sub' e0) (sub' e1)
        S.CUnOp op e -> S.CUnOp op (sub' e)
        S.CTernOp op x y z -> S.CTernOp op (sub' x) (sub' y) (sub' z)
        -- induction, but rename binder if it conflicts with fv(e)
        Lam b@(S.Binder v'@(S.Var _ v'Pri) t) body
          | v /= v' ->
            let fvE = fv e
                fvB = fv body
             in if v'Pri `Set.member` fvE
                  then
                    let v'' = fresh v' v (fvE `Set.union` fvB)
                        body' = sub' $ sub (S.CVar v'') (S.CVar v') body
                     in S.mkCLam (S.Binder v'' t) body'
                  else S.mkCLam b (sub' body)
        -- irrelevent base cases
        _ -> e'
sub _ _ _ = error "can't substitute for anything but a variable!"

--  Normal order reduction.
reduce :: S.CoreExpr -> Evaluation S.CoreExpr
reduce expr = case expr of
  -- (1) leftmost, outermost
  App e0 e1 -> case e0 of
    -- (1a) function application, beta reduce
    Lam (S.Binder v _) body -> do
      -- traceM (show expr)
      e <- reduce (sub e1 (S.CVar v) body)
      -- traceM (show e)
      pure e
    -- (1b) binders have a semantics of their own: they may be applied
    -- to terms, in which case they simply abstract a free variable.
    S.CBind b -> reduce (S.mkCLam b e1)
    -- (1c) normal form for lhs (free var), goto rhs
    S.CVar {} -> do
      e1' <- reduce e1
      pure (S.mkCApp e0 e1')
    -- (1d) lhs can be reduced
    _ -> do
      e0' <- reduce e0
      case e0' of
        -- (1d.1) normal form for lhs (app), goto rhs
        App {} -> do
          e1' <- reduce e1
          pure (S.mkCApp e0' e1')
        -- (1d.2) otherwise goto top
        _ -> reduce (S.mkCApp e0' e1)
  -- (2) simplify lambda body
  Lam b body -> do
    body' <- reduce body
    pure $ S.mkCLam b body'
  -- (3) operators
  S.CUnOp op e -> do
    e' <- reduce e
    pure $
      S.mkCBool $ case op of
        S.Neg -> not $ bool e'
  S.CBinOp op e0 e1 -> do
    e0' <- reduce e0
    e1' <- reduce e1
    pure $
      S.mkCBool $ case op of
        S.Eq -> e0' @= e1'
        S.NEq -> e0' @!= e1'
        S.And -> bool e0' && bool e1'
        S.Or -> bool e0' || bool e1'
  S.CTernOp S.Cond x y z -> do
    x' <- reduce x
    case x' of
      S.CLit S.LBool {} -> reduce $ if bool x' then y else z
      _ -> throwError (NotTruthy x)
  -- (4) var/literal
  _ -> pure expr

-- assumes e0 and e1 are in normal form
alphaEq :: S.CoreExpr -> S.CoreExpr -> Bool
alphaEq e0 e1 = case (e0, e1) of
  (Lam (S.Binder v0 _) body0, Lam (S.Binder v1 _) body1) ->
    let v0' = S.CVar v0
        v1' = S.CVar v1
     in sub v1' v0' body0 @= body1 || sub v0' v1' body1 @= body0
  (App e0a e0b, App e1a e1b) -> e0a @= e1a && e0b @= e1b
  (S.CVar v0, S.CVar v1) -> v0 == v1
  (S.CLit l0, S.CLit l1) -> l0 == l1
  _ -> False

(@=) :: S.CoreExpr -> S.CoreExpr -> Bool
e0 @= e1 = alphaEq e0 e1

(@!=) :: S.CoreExpr -> S.CoreExpr -> Bool
e0 @!= e1 = not (e0 @= e1)

eval :: S.CoreExpr -> Either EvalError S.CoreExpr
eval e = runIdentity $ runExceptT $ reduce e

confluent :: S.CoreExpr -> S.CoreExpr -> Bool
confluent e0 e1 = case (eval e0, eval e1) of
  (Right e1', Right e0') -> e0' @= e1'
  _ -> False

(*=) :: S.CoreExpr -> S.CoreExpr -> Bool
e0 *= e1 = confluent e0 e1

(*!=) :: S.CoreExpr -> S.CoreExpr -> Bool
e0 *!= e1 = not (e0 *= e1)
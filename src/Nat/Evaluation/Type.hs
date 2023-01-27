{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Nat.Evaluation.Type where

import Control.Monad (filterM, foldM, (<=<), (>=>))
import Control.Monad.Except
  ( Except,
    ExceptT,
    MonadError (throwError),
    catchError,
    liftEither,
    runExcept,
    runExceptT,
  )
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader
  ( MonadReader (ask, local),
    ReaderT (runReaderT),
    asks,
  )
import Control.Monad.State
  ( MonadState (get, put),
    StateT,
    evalStateT,
    gets,
    modify,
    state,
  )
import Control.Monad.Trans (lift)
import Data.Bool (Bool (True))
import Data.Either (isRight)
import Data.Foldable (toList)
import Data.Functor ((<&>))
import Data.List (find, partition)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import qualified Data.Set as Set
import Debug.Trace (trace, traceM)
import Nat.Context hiding (fresh, fresh')
import qualified Nat.Context as C
import Nat.Control (foldM1)
import Nat.Evaluation.Context
import Nat.Inference
import Nat.Reduction
import qualified Nat.Syntax.Surface as S
import Nat.Syntax.Type
import Nat.Unification
import Nat.Viz
import Nat.Walk
import Text.PrettyPrint (vcat, (<+>))

type TypeInferT r = InferT Type S.Expr r

type TypeEnv = Signature Type

-------------------------------------------------------------------------------
-- Inference - constraint generation
-------------------------------------------------------------------------------

-- | Extend type environment
extend :: TypeEnv -> (Var, Type) -> TypeEnv
extend env (v, t) = Map.insert v t env

inTypeEnv :: (Var, Type) -> TypeInferT a -> TypeInferT a
inTypeEnv t = local (`extend` t)

-- | Lookup type in the environment
checkTy :: Var -> TypeInferT Type
checkTy x = do
  env <- ask
  case Map.lookup x env of
    Nothing -> throwError $ IUnboundVariable x env
    Just t -> return t

tyOp op = case op of
  S.Add -> TyFun tyInt (TyFun tyInt tyInt)
  S.Sub -> TyFun tyInt (TyFun tyInt tyInt)
  S.Mul -> TyFun tyInt (TyFun tyInt tyInt)
  S.And -> TyFun tyBool (TyFun tyBool tyBool)
  S.Or -> TyFun tyBool (TyFun tyBool tyBool)
  _ -> TyFun (mkTv "Z") (TyFun (mkTv "Z") tyBool)

principalInEnv e v t = inTypeEnv (v, t) (principal e)

type Case = (S.Binder S.Expr, S.Expr)

-- | (1) mint fresh tvars for the vars in the pattern
--   (2) constrain the pattern under these tvars
--   (3) constrain the expr under these tvars
--   (4) constrain the inferred type of the pattern
--       and the pattern's type annotation to be equal
--   (5) return the pattern and body's types
--       alongside constraints incurred along the way
principalCase ::
  (S.Binder S.Expr, S.Expr) ->
  InferT Type S.Expr ((Type, Type), [Constraint Type])
principalCase (S.Binder p t, expr) = do
  let vs = Set.toList $ fv p
  tvs <- mapM (const fresh) vs
  let env = Map.fromList (zip vs tvs)

  (pT, pCs) <- local (Map.union env) (principal p)
  (tv, cs) <- local (Map.union env) (principal expr)

  return ((t, tv), (pT, t) : pCs ++ cs)

isVar = \case TyVar {} -> True; _ -> False

-- | Make a church tree from an ETree and rename its variables.
mkChurchTree :: Type -> S.Tree S.Expr -> TypeInferT S.Expr
mkChurchTree branchTy t = do
  let t' = S.mkTypedChurchTree branchTy t
  i <- gets names
  let (t'', i') = runFreshT i $ renameExprTypes t'
  modify (\s -> s {names = i'})
  return t''

freshIfNil :: Type -> TypeInferT Type
freshIfNil t = case t of
  TyNil -> fresh
  _ -> return t

instance Inferrable Type S.Expr where
  fresh = mkTv' <$> fresh'

  -- ETyCases get special treatment, namely:
  -- if the scrutinee has a non-variable principal
  -- type at this step, then we can determine the
  -- matched pattern/body, and thus the principal type
  -- of the whole case expression, otherwise we defer
  principal e = case e of
    S.ETyCase {} -> do
      (t, cs) <- principal' e
      let (TyTyCase bT ts) = t
      if isVar bT
        then return (t, cs)
        else case find (unifiable bT . fst) ts of
          Nothing -> throwError $ IInexhaustiveCase t
          Just (pT, eT) -> do
            s <- liftEither (liftUnify ((pT, bT) : cs) e)
            return (inEnv s eT, cs)
    _ -> principal' e

  constrain' expr = case expr of
    S.ELit (S.LInt _) -> return (tyInt, [])
    S.ELit (S.LBool _) -> return (tyBool, [])
    S.EVar v -> do
      t <- checkTy v
      return (t, [])
    S.ELam (S.Binder v t) e -> do
      dom <- freshIfNil t
      (ran, cs) <- principalInEnv e v dom
      return (dom `TyFun` ran, cs)
    S.EApp e0 e1 -> do
      (t0, c0) <- principal e0
      (t1, c1) <- principal e1
      tv <- fresh
      return (tv, c0 ++ c1 ++ [(t0, t1 `TyFun` tv)])
    S.EFix v e -> do
      tv <- fresh
      principalInEnv e v (TyFun tv tv)
    S.EBinOp op e0 e1 -> do
      (t0, c0) <- principal e0
      (t1, c1) <- principal e1
      tv <- fresh
      let (TyFun a (TyFun b c)) = tyOp op
      return (tv, c0 ++ c1 ++ [(t0, a), (t1, b), (c, tv)])
    S.ECond x y z -> do
      (tX, cX) <- principal x
      (tY, cY) <- principal y
      (tZ, cZ) <- principal z
      tv <- fresh
      return (tv, cX ++ cY ++ cZ ++ [(tX, tyBool), (tY, tv), (tZ, tv)])
    S.ETup es -> do
      tv <- fresh
      cs <- mapM principal es
      let (ts, cs') = unzip cs
      return (tv, (tv, TyTup ts) : concat cs')
    S.EWild -> return (TyWild, [])
    S.ETyCase scrutinee cases -> do
      tv <- fresh
      (scrutineeType, eCs) <- principal scrutinee
      (caseTypes, caseConstraints) <- (mapM principalCase >=> (return . unzip)) cases

      let cs = [(scrutineeType, tv), (scrutineeType, TyUnion (Set.fromList [t | (S.Binder _ t) <- map fst cases]))]

      return (TyTyCase tv caseTypes, concat [cs, eCs, concat caseConstraints])
    S.ETree t -> do
      (branchTys, branchConstrs) <- unzip <$> mapM principal (toList t)
      branchTy <- TyFun (refine branchTys) <$> fresh
      -- use the name supply of the inference monad
      -- to instantiate a fresh typed church tree
      t' <- mkChurchTree branchTy t
      (tv, cs) <- principal t'
      return (tv, concat $ cs : branchConstrs)
    S.ETree t -> (principal <=< mkChurchTree TyNil) t
    S.EUndef -> return (TyUndef, [])
    _ -> throwError $ IUnconstrainable expr

refine ts = if uniform ts then head ts else TyUnion $ Set.fromList ts
  where
    uniform xs = all (== head xs) (tail xs)

-------------------------------------------------------------------------------
-- Inference - constraint unification
-------------------------------------------------------------------------------
instance Unifiable Type where
  -- Invariants, which remove the need for recursion in unify:
  -- (1) no function constraints: they're expanded
  -- (2) no tuple constraints: also expanded
  invariants (a, b) = case (a, b) of
    (TyTup ts, TyTup ts') ->
      if length ts == length ts'
        then expandInvariants (zip ts ts')
        else throwContextualError (NotUnifiable a b)
    (TyFun d r, TyFun d' r') -> do
      cs <- invariants (d, d')
      cs' <- invariants (r, r')
      return (cs ++ cs')
    c -> return [c]
    where
      isTv = \case
        TyVar {} -> True
        _ -> False

  unify t0 t1 = case (t0, t1) of
    _ | t0 == t1 -> eliminate
    (TyVar v, t) -> return $ mkEnv v t
    (t, TyVar v) -> return $ mkEnv v t
    (TyWild, _) -> eliminate
    (_, TyWild) -> eliminate
    (TyUnion ts, t) | some t ts -> eliminate
    (t, TyUnion ts) | some t ts -> eliminate
    _ -> throwContextualError $ NotUnifiable t0 t1
    where
      eliminate = return mempty
      some t ts = or (unifiable t <$> Set.toList ts)

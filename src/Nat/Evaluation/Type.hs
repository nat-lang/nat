{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Nat.Evaluation.Type where

import Control.Monad ((<=<))
import Control.Monad.Error (MonadError (catchError))
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
import Nat.Evaluation.Context
import Nat.Inference
import qualified Nat.Syntax.Surface as S
import Nat.Syntax.Type
import Nat.Unification
import Nat.Viz
import Nat.Walk
import Text.PrettyPrint (vcat, (<+>))

-------------------------------------------------------------------------------
-- Classes
-------------------------------------------------------------------------------

type TypeConstrainT r = ConstrainT Type S.Expr r

type TypeEnv = ConstraintEnv Type

instance Pretty (Constraint Type) where
  ppr p (a0, a1) = ppr p a0 <+> text "=" <+> ppr p a1

instance Pretty [Constraint Type] where
  ppr p cs = vcat $ fmap ((text "\t" <>) . ppr p) cs

-------------------------------------------------------------------------------
-- Inference
-------------------------------------------------------------------------------

-- | Extend type environment
extend :: TypeEnv -> (Var, Type) -> TypeEnv
extend env (v, t) = Map.insert v t env

inTypeEnv :: (Var, Type) -> TypeConstrainT a -> TypeConstrainT a
inTypeEnv t = local (`extend` t)

-- | Lookup type in the environment
checkTy :: Var -> TypeConstrainT Type
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

constrainWith e v t = inTypeEnv (v, t) (principal e)

type Case = (S.Binder S.Expr, S.Expr)

-- | (1) mint fresh tvars for the vars in the pattern
--   (2) constrain the pattern under these tvars
--   (3) constrain the expr under these tvars
--   (4) constrain the inferred type of the pattern
--       and the pattern's type annotation to be equal
--   (5) return the sumtype of the pattern and body's types
--       alongside constraints incurred along the way
constrainCase ::
  (S.Binder S.Expr, S.Expr) ->
  ConstrainT Type S.Expr ((Type, Type), [Constraint Type])
constrainCase (S.Binder p t, expr) = do
  let vs = Set.toList $ fv p
  tvs <- mapM (const fresh) vs
  let env = Map.fromList (zip vs tvs)

  (pT, pCs) <- local (Map.union env) (principal p)
  (tv, cs) <- local (Map.union env) (principal expr)

  return ((t, tv), (pT, t) : pCs ++ cs)

isVar = \case TyVar {} -> True; _ -> False

mkChurchTree t = do
  let t' = S.mkChurchTree t
  state (flip runFreshT $ renameExprTypes t')

freshIfNil :: Type -> TypeConstrainT Type
freshIfNil t = case t of
  TyNil -> fresh
  _ -> return t

liftUnion t' = case t' of
  TyUnion {} -> t'
  _ -> mkTyUnion [t']

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
            s <- liftEither (unify' ((pT, bT) : cs) e)
            return (inEnv s eT, cs)
    _ -> principal' e

  constrain expr = case expr of
    S.ELit (S.LInt _) -> return (tyInt, [])
    S.ELit (S.LBool _) -> return (tyBool, [])
    S.EVar v -> do
      t <- checkTy v
      return (t, [])
    S.ELam (S.Binder v t) e -> do
      range <- freshIfNil t

      (dom, cs) <- constrainWith e v range
      return (range `TyFun` dom, cs)
    S.EApp e0 e1 -> do
      (t0, c0) <- principal e0
      (t1, c1) <- principal e1
      tv <- fresh
      return (tv, c0 ++ c1 ++ [(t0, t1 `TyFun` tv)])
    S.EFix v e -> do
      tv <- fresh
      constrainWith e v (TyFun tv tv)
    S.EBinOp op e0 e1 -> do
      (t0, c0) <- principal e0
      (t1, c1) <- principal e1
      tv <- fresh
      tv' <- fresh
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
    S.ETyCase e cases -> do
      tv <- fresh
      (et, eCs) <- principal e
      cTs <- mapM constrainCase cases

      let (cTs', cCs) = unzip cTs
      let cs = [(et, tv), (et, TyUnion (Set.fromList [t | (S.Binder _ t) <- map fst cases]))]

      return (TyTyCase tv cTs', concat [cs, eCs, concat cCs])
    S.ETree t -> do
      t' <- mkChurchTree t
      principal t'
    S.EUndef -> return (TyUndef, [])
    _ -> throwError $ IUnconstrainable expr

refine ts = if uniform ts then head ts else TyUnion $ Set.fromList ts
  where
    uniform xs = all (== head xs) (tail xs)

-------------------------------------------------------------------------------
-- Unification
-------------------------------------------------------------------------------

instance Unifiable Type where
  unifyMany ps = unifyMany' ps
    where
      -- s <- unifyUnions ps
      -- traceM ("=> " ++ show s)
      -- traceM ("==> " ++ show (inEnv s ps))
      -- z' <- unifyMany' (inEnv s ps)
      -- traceM ("===> " ++ show z')
      -- traceM ("====> " ++ show (s <.> z'))
      -- return (s <.> z')

      unifyUnions :: [Pair Type] -> UnifyM Type
      unifyUnions pairs = do
        let (fns, _) = partition isFnSub pairs
            fns' = map leftOrd fns
        traceM ("constraints ... " ++ show pairs)
        traceM (" functional ... " ++ show fns')
        overloadedEnv <- merge fns'
        traceM ("post merge ... " ++ show overloadedEnv)
        traceM ("post sub ... " ++ show (inEnv overloadedEnv pairs))
        return overloadedEnv

      merge :: [Pair Type] -> UnifyM Type
      merge pairs = case pairs of
        ((a@(TyVar v), TyFun r d) : (a', TyFun r' d') : ps) | a <=> a',
                                                              d <=> d',
                                                              r <!> r' -> do
          let overloadedFn = TyFun (mkTyUnion [r, r']) d
              sub = mkEnv v overloadedFn
          us <- merge (inEnv sub ps)
          return (sub <.> us)
        _ -> return mempty

      leftOrd = \case
        (b@TyFun {}, a) -> (a, b)
        p -> p
      isFnSub = \case
        (TyVar {}, TyFun {}) -> True
        (TyFun {}, TyVar {}) -> True
        _ -> False

  unify t0 t1 = case (t0, t1) of
    _ | t0 == t1 -> eliminate
    (TyWild, _) -> eliminate
    (_, TyWild) -> eliminate
    (TyVar v, t) -> return $ mkEnv v t
    (t, TyVar v) -> return $ mkEnv v t
    (TyFun r d, TyFun r' d') -> do
      u <- unify r r'
      u' <- unify d d'
      return (u <.> u')
    (TyUnion ts, t) | some t ts -> eliminate
    (t, TyUnion ts) | some t ts -> eliminate
    (TyTup ts, TyTup ts') | length ts == length ts' -> unifyMany (zip ts ts')
    _ -> throwError $ NotUnifiable t0 t1
    where
      eliminate = return mempty
      some t ts = or (unifiable t <$> Set.toList ts)
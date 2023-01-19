{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Nat.Evaluation.Type where

import Control.Monad (foldM, (<=<), (>=>))
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
import Nat.Reduction
import qualified Nat.Syntax.Surface as S
import Nat.Syntax.Type
import Nat.Unification
import Nat.Viz
import Nat.Walk
import Text.PrettyPrint (vcat, (<+>))

-------------------------------------------------------------------------------
-- Classes
-------------------------------------------------------------------------------

type TypeInferT r = InferT Type S.Expr r

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
--   (5) return the sumtype of the pattern and body's types
--       alongside constraints incurred along the way
constrainCase ::
  (S.Binder S.Expr, S.Expr) ->
  InferT Type S.Expr ((Type, Type), [Constraint Type])
constrainCase (S.Binder p t, expr) = do
  let vs = Set.toList $ fv p
  tvs <- mapM (const fresh) vs
  let env = Map.fromList (zip vs tvs)

  (pT, pCs) <- local (Map.union env) (principal p)
  (tv, cs) <- local (Map.union env) (principal expr)

  return ((t, tv), (pT, t) : pCs ++ cs)

isVar = \case TyVar {} -> True; _ -> False

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
    S.ETyCase e cases -> do
      tv <- fresh
      (et, eCs) <- principal e
      cTs <- mapM constrainCase cases

      let (cTs', cCs) = unzip cTs
      let cs = [(et, tv), (et, TyUnion (Set.fromList [t | (S.Binder _ t) <- map fst cases]))]

      return (TyTyCase tv cTs', concat [cs, eCs, concat cCs])
    S.ETree t -> (principal <=< mkChurchTree TyNil) t
    S.ETree t -> do
      (branchTys, branchConstrs) <- unzip <$> mapM principal (toList t)
      branchTy <- TyFun (refine branchTys) <$> fresh
      -- use the name supply of the inference monad
      -- to instantiate a fresh typed church tree
      t' <- mkChurchTree branchTy t
      (tv, cs) <- principal t'
      return (tv, concat $ cs : branchConstrs)
    S.EUndef -> return (TyUndef, [])
    _ -> throwError $ IUnconstrainable expr

refine ts = if uniform ts then head ts else TyUnion $ Set.fromList ts
  where
    uniform xs = all (== head xs) (tail xs)

-------------------------------------------------------------------------------
-- Unification
-------------------------------------------------------------------------------

join :: Type -> Type -> Type
join (TyUnion u) (TyUnion u') = TyUnion (Set.union u u')
join (TyUnion u) t = TyUnion (Set.insert t u)
join t (TyUnion u) = TyUnion (Set.insert t u)
join t t' = mkTyUnion [t, t']

maybeOverload :: Type -> Type -> [Pair Type] -> UnifyM Type
maybeOverload v@TyVar {} f@(TyFun d r) cs = do
  p <- foldM overload (v, f) cs0

  let (_, f') = p
  let (TyVar tv) = v

  return (Map.singleton tv f')
  where
    (cs0, cs1) = partition domClash cs
    domClash (v', TyFun d' r') = v == v' && d <!> d'
    domClash (TyFun d' r', v') = v == v' && d <!> d'
    domClash _ = False

    overload :: Pair Type -> Pair Type -> UnifyT Type (Pair Type)
    overload p (v', f'@TyFun {}) = overload p (f', v')
    overload (_, TyFun d r) (TyFun d' r', _) = do
      u <- unify r r'

      let ran = inEnv u r
      let dom = join d d'

      return (v, TyFun dom ran)
maybeOverload v t _ = unify v t

instance Unifiable Type where
  unifyMany cs = case cs of
    [] -> pure mempty
    ((a0, a1) : cs') -> do
      u <- unify' a0 a1 cs'
      us <- unifyMany (inEnv u cs')
      return (u <.> us)
    where
      unify' v@TyVar {} f@TyFun {} cs = maybeOverload v f cs
      unify' f@TyFun {} v@TyVar {} cs = maybeOverload v f cs
      unify' a0 a1 _ = unify a0 a1

  unify t0 t1 = case (t0, t1) of
    _ | t0 == t1 -> eliminate
    (TyWild, _) -> eliminate
    (_, TyWild) -> eliminate
    (TyVar v, t) -> return $ mkEnv v t
    (t, TyVar v) -> return $ mkEnv v t
    (TyFun d r, TyFun d' r') -> do
      u <- unify d d'
      u' <- unify r r'
      return (u <.> u')
    (TyUnion ts, t) | some t ts -> eliminate
    (t, TyUnion ts) | some t ts -> eliminate
    (TyTup ts, TyTup ts') | length ts == length ts' -> unifyMany (zip ts ts')
    _ -> throwError $ NotUnifiable t0 t1
    where
      eliminate = return mempty
      some t ts = or (unifiable t <$> Set.toList ts)
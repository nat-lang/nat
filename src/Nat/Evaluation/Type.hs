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

type TypeEnv = ConstraintEnv Type

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

constrainInEnv e v t = inTypeEnv (v, t) (constrain e)

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

  (pT, pCs) <- local (Map.union env) (constrain p)
  (tv, cs) <- local (Map.union env) (constrain expr)

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
      (ran, cs) <- constrainInEnv e v dom
      return (dom `TyFun` ran, cs)
    S.EApp e0 e1 -> do
      (t0, c0) <- constrain e0
      (t1, c1) <- constrain e1
      tv <- fresh
      return (tv, c0 ++ c1 ++ [(t0, t1 `TyFun` tv)])
    S.EFix v e -> do
      tv <- fresh
      constrainInEnv e v (TyFun tv tv)
    S.EBinOp op e0 e1 -> do
      (t0, c0) <- constrain e0
      (t1, c1) <- constrain e1
      tv <- fresh
      let (TyFun a (TyFun b c)) = tyOp op
      return (tv, c0 ++ c1 ++ [(t0, a), (t1, b), (c, tv)])
    S.ECond x y z -> do
      (tX, cX) <- constrain x
      (tY, cY) <- constrain y
      (tZ, cZ) <- constrain z
      tv <- fresh
      return (tv, cX ++ cY ++ cZ ++ [(tX, tyBool), (tY, tv), (tZ, tv)])
    S.ETup es -> do
      tv <- fresh
      cs <- mapM constrain es
      let (ts, cs') = unzip cs
      return (tv, (tv, TyTup ts) : concat cs')
    S.EWild -> return (TyWild, [])
    S.ETyCase e cases -> do
      tv <- fresh
      (et, eCs) <- constrain e
      cTs <- mapM constrainCase cases

      let (cTs', cCs) = unzip cTs
      let cs = [(et, tv), (et, TyUnion (Set.fromList [t | (S.Binder _ t) <- map fst cases]))]

      return (TyTyCase tv cTs', concat [cs, eCs, concat cCs])
    S.ETree t -> (constrain <=< mkChurchTree TyNil) t
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
    -- (_, TyVar {}) | not (isTv a) -> return [(b, a)]
    c -> return [c]
    where
      isTv = \case
        TyVar {} -> True
        _ -> False

  -- Before we pass the constraint to the simple unification routine,
  -- check if we have overloaded functional type variables, and if we
  -- do then join their domains into union types in the returned signature.
  unify' cs c = case c of
    (v@TyVar {}, f@TyFun {}) -> maybeOverload v f
    (f@TyFun {}, v@TyVar {}) -> maybeOverload v f
    (a0, a1) -> unify a0 a1
    where
      maybeOverload :: Type -> Type -> UnifyM Type
      maybeOverload tv@(TyVar v) fn@(TyFun d r) = do
        domClashes <- filterM (domClash tv d) cs

        if not $ null domClashes
          then traceM ("dom clash for " ++ show tv ++ " = " ++ show fn)
          else traceM (" no dom clash for " ++ show tv ++ " = " ++ show fn)

        oFn <- foldM overload fn domClashes
        return (Map.singleton v oFn)
      maybeOverload v t = unify v t

      domClash :: Type -> Type -> Pair Type -> UnifyT Type Bool
      domClash v d p = case p of
        (v', TyFun d' _) | v == v' -> test d d'
        (TyFun d' _, v') | v == v' -> test d d'
        _ -> return False
        where
          test d d' = do
            s <- get
            return $ not (unifiableIn s ((d, d') : cs))

      overload :: Type -> Pair Type -> UnifyT Type Type
      overload fn (v', f'@TyFun {}) = overload fn (f', v')
      overload (TyFun d r) (TyFun d' r', _) = do
        u <- unify r r'
        let ran = inEnv u r
        let dom = join d d'
        return (TyFun dom ran)

      join :: Type -> Type -> Type
      join (TyUnion u) (TyUnion u') = TyUnion (Set.union u u')
      join (TyUnion u) t = TyUnion (Set.insert t u)
      join t (TyUnion u) = TyUnion (Set.insert t u)
      join t t' = mkTyUnion [t, t']

  unify t0 t1 = case (t0, t1) of
    _ | t0 == t1 -> eliminate
    (TyWild, _) -> eliminate
    (_, TyWild) -> eliminate
    (TyVar v, t) -> return $ mkEnv v t
    (t, TyVar v) -> return $ mkEnv v t
    (TyUnion ts, t) | some t ts -> eliminate
    (t, TyUnion ts) | some t ts -> eliminate
    _ -> throwContextualError $ NotUnifiable t0 t1
    where
      eliminate = return mempty
      some t ts = or (unifiable t <$> Set.toList ts)

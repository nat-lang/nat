{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Mean.Evaluation.Type where

import Control.Monad (liftM, replicateM)
import Control.Monad.Except
  ( Except,
    ExceptT,
    MonadError (throwError),
    liftEither,
    replicateM,
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
import Data.Either (isRight)
import Data.Foldable (toList)
import Data.List (find, foldl', nub)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import qualified Data.Set as Set
import Debug.Trace (trace, traceM)
import Mean.Context
import Mean.Evaluation.Context
import Mean.Inference
import Mean.Syntax.Logic
import qualified Mean.Syntax.Surface as S
import Mean.Syntax.Type
import Mean.Unification
import Mean.Viz
import Mean.Walk
import Text.PrettyPrint (vcat, (<+>))

-------------------------------------------------------------------------------
-- Classes
-------------------------------------------------------------------------------

type TypeConstrainT r = ConstrainT Type S.Expr r

type TypeConstrain = TypeConstrainT Type

type TypeEnv = ConstraintEnv Type

instance Pretty (Constraint Type) where
  ppr p (a0, a1) = ppr p a0 <+> text "=" <+> ppr p a1

instance Pretty [Constraint Type] where
  ppr p cs = vcat $ fmap ((text "\t" <>) . ppr p) cs

instance Substitutable Type Type where
  sub v t = walk $ \t' -> case t' of
    TyVar v' | v' == v -> t
    _ -> t'

instance Contextual Type where
  fv t = case t of
    TyCon {} -> Set.empty
    TyWild -> Set.empty
    TyVar a -> Set.singleton a
    TyFun t0 t1 -> fv t0 `Set.union` fv t1
    TyUnion ts -> foldMap fv ts
    TyTyCase b cs -> let (csL, csR) = unzip cs in Set.unions [fv b, fv csL, fv csR]
    TyQuant (Univ vs t) -> fv t `Set.difference` Set.fromList vs

freshTv :: Monad m => FreshT m Type
freshTv = do
  i <- fresh
  let c = 'A' : show i
  pure $ TyVar (Var c c)

freshTv' :: TypeConstrainT Type
freshTv' = do
  tv <- lift freshTv
  pure tv

instance Renamable Type where
  next (Var vPub _) = TyVar <$> (Var vPub . show <$> fresh)

  rename' vs t = flip walkM t $ \case
    TyVar v | Set.member v vs -> next v
    TyNil -> freshTv
    t' -> pure t'

-------------------------------------------------------------------------------
-- Inference
-------------------------------------------------------------------------------

-- | Extend type environment
extend :: TypeEnv -> (Var, Type) -> TypeEnv
extend env (v, t) = Map.insert v t env

inTypeEnv :: (Var, Type) -> TypeConstrainT a -> TypeConstrainT a
inTypeEnv t = local (`extend` t)

-- | Lookup type in the environment
checkTy :: Var -> TypeConstrain
checkTy x = do
  env <- ask
  case Map.lookup x env of
    Nothing -> throwError $ IUnboundVariable x env
    Just t -> pure t

tyOp op = case op of S.Add -> tyInt; S.Sub -> tyInt; S.Mul -> tyInt; _ -> tyBool

constrainWith e v t = inTypeEnv (v, t) (principal e)

type Case = (S.Binder S.Expr, S.Expr)

-- | (1) mint fresh tvars for the vars in the pattern
--   (2) constrain the pattern under these tvars
--   (3) constrain the expr under these tvars
--   (4) constrain the inferred type of the pattern
--       the pattern's type to be equal
--   (5) return the sum of the pattern and body's types +
--       constraints incurred along the way
constrainCase ::
  (S.Binder S.Expr, S.Expr) ->
  ConstrainT Type S.Expr ((Type, Type), [Constraint Type])
constrainCase (S.Binder p t, expr) = do
  let vs = Set.toList $ fv p
  tvs <- mapM (const freshTv) vs
  let env = Map.fromList (zip vs tvs)

  (pT, pCs) <- local (Map.union env) (principal p)
  (tv, cs) <- local (Map.union env) (principal expr)

  return ((t, tv), (pT, t) : pCs ++ cs)

isVar = \case TyVar {} -> True; _ -> False

instance Inferrable Type S.Expr where
  -- ETyCases get special treatment
  principal e = case e of
    S.ETyCase {} -> do
      (t, cs) <- principal' e
      let (TyTyCase bT ts) = t
      if isVar bT
        then pure (t, cs)
        else case find (unifiable bT . fst) ts of
          Nothing -> throwError $ IInexhaustiveCase t
          Just (pT, eT) -> do
            s <- liftEither (unify' ((pT, bT) : cs) e)
            pure (inEnv s eT, cs)
    _ -> principal' e

  constrain expr = case expr of
    S.ELit (S.LInt _) -> return (tyInt, [])
    S.ELit (S.LBool _) -> return (tyBool, [])
    S.EVar v -> do
      t <- checkTy v
      return (t, [])
    S.ELam (S.Binder v t) e -> do
      (t', cs) <- constrainWith e v t
      return (t `TyFun` t', cs)
    S.EFix v e -> do
      tv <- freshTv'
      constrainWith e v (TyFun tv tv)
    S.EApp e0 e1 -> do
      (t0, c0) <- principal e0
      (t1, c1) <- principal e1
      tv <- freshTv
      return (tv, c0 ++ c1 ++ [(t0, t1 `TyFun` tv)])
    S.EBinOp op e0 e1 -> do
      (t0, c0) <- principal e0
      (t1, c1) <- principal e1
      tv <- freshTv
      return (tv, c0 ++ c1 ++ [(t0, t1), (tyOp op, tv)])
    S.ECond x y z -> do
      (tX, cX) <- principal x
      (tY, cY) <- principal y
      (tZ, cZ) <- principal z
      tv <- freshTv
      return (tv, cX ++ cY ++ cZ ++ [(tX, tyBool), (tY, tv), (tZ, tv)])
    S.ETup es -> do
      tv <- freshTv
      cs <- mapM principal es
      let (ts, cs') = unzip cs
      return (tv, (tv, TyTup ts) : concat cs')
    S.EWildcard -> return (TyWild, [])
    S.ETyCase e cases -> do
      tv <- freshTv
      (et, eCs) <- principal e
      cTs <- mapM constrainCase cases

      let (cTs', cCs) = unzip cTs
      let cs = [(et, tv), (et, TyUnion (Set.fromList [t | (S.Binder _ t) <- map fst cases]))]

      return (TyTyCase tv cTs', concat [cs, eCs, concat cCs])
    S.ETree t -> do
      eTs <- mapM principal (toList t)
      let (tEs, cEs) = unzip eTs
      tv <- freshTv
      let bTy = TyFun (refine tEs) tv
      let t' = S.mkTypedChurchTree bTy t
      (tv, cs) <- principal t'
      return (tv, concat $ cs : cEs)
    S.EUndef -> pure (TyUndef, [])
    _ -> throwError $ IUnconstrainable expr

refine ts = if uniform ts then head ts else TyUnion $ Set.fromList ts
  where
    uniform xs = all (== head xs) (tail xs)

-------------------------------------------------------------------------------
-- Unification
-------------------------------------------------------------------------------

instance Unifiable Type where
  unify t0 t1 =
    let some t ts = or (unifiable t <$> Set.toList ts)
     in case (t0, t1) of
          _ | t0 == t1 -> pure mempty
          (TyWild, _) -> pure mempty
          (_, TyWild) -> pure mempty
          (TyVar v, t) -> pure $ mkEnv v t
          (t, TyVar v) -> pure $ mkEnv v t
          (TyFun t0 t1, TyFun t0' t1') -> do
            u0 <- unify t0 t0'
            u1 <- unify t1 t1'
            pure (u0 <.> u1)
          (TyUnion ts, t) | some t ts -> pure mempty
          (t, TyUnion ts) | some t ts -> pure mempty
          (TyTup ts, TyTup ts') | length ts == length ts' -> unifyMany (zip ts ts')
          _ -> throwError $ NotUnifiable t0 t1

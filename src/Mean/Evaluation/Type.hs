{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Mean.Evaluation.Type where

import Control.Monad (liftM, replicateM)
import Control.Monad.Except
  ( Except,
    ExceptT,
    MonadError (throwError),
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
import Data.List (foldl', nub)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import qualified Data.Set as Set
import Debug.Trace (trace, traceM)
import Mean.Inference
import Mean.Syntax.Logic
import qualified Mean.Syntax.Surface as S
import Mean.Syntax.Type
import Mean.Unification
import Mean.Var
import Mean.Viz
import Prelude hiding ((<*>))

-------------------------------------------------------------------------------
-- Classes
-------------------------------------------------------------------------------

-- | Constraint generation state
newtype ConstraintState = CState {count :: Int} deriving (Show)

type TypeConstrainT a = ConstrainT Type S.Expr ConstraintState a

type TypeConstrain = TypeConstrainT Type

type TypeEnv = ConstraintEnv Type

letters :: [String]
letters = [1 ..] >>= flip replicateM ['A' .. 'Z']

-- | Counter for fresh variables
initConstrain :: ConstraintState
initConstrain = CState {count = 0}

instance Substitutable Type Type where
  substitute t0 t1 = case (t0, t1) of
    (_, TyCon {}) -> t1
    (s, TyVar a) -> Map.findWithDefault t1 a s
    (s, TyFun t0' t1') -> let sub' = substitute s in sub' t0' `TyFun` sub' t1'
    (s, TyUnion ts) -> TyUnion $ Set.fromList (substitute s <$> Set.toList ts)
    (s, TyQuant (Univ as t)) -> TyQuant $ Univ as $ substitute s' t
      where
        s' = foldr Map.delete s as

instance Contextual Type where
  fv t = case t of
    TyCon {} -> Set.empty
    TyVar a -> Set.singleton a
    TyFun t0 t1 -> fv t0 `Set.union` fv t1
    TyUnion ts -> foldMap fv ts
    TyQuant (Univ vs t) -> fv t `Set.difference` Set.fromList vs

-------------------------------------------------------------------------------
-- Inference
-------------------------------------------------------------------------------

-- | Quantify a type universally over its
closeOver :: Type -> Type
closeOver = normalize . generalize mkCEnv

instantiate :: Type -> TypeConstrain
instantiate (TyQuant (Univ as t)) = do
  as' <- mapM (const fresh) as
  let e = Map.fromList $ zip as as'
  return $ substitute e t
instantiate t = error ("can only instantiate a quantified type, but got " ++ (show t))

generalize :: TypeEnv -> Type -> Type
generalize env t = TyQuant $ Univ as t
  where
    as = Set.toList $ fv t `Set.difference` fv env

normalize :: Type -> Type
normalize (TyQuant (Univ _ body)) = TyQuant $ Univ (map snd ord) (normtype body)
  where
    ord = zip (nub $ fvs body) (map mkVar letters)
    fvs = Set.toList . fv
    normtypes = fmap normtype

    normtype (TyVar a) =
      case Prelude.lookup a ord of
        Just x -> TyVar x
        Nothing -> error "type variable not in signature"
    normtype (TyCon a) = TyCon a
    normtype (TyFun a b) = TyFun (normtype a) (normtype b)
    normtype (TyTup ts) = TyTup (normtypes ts)
    normtype (TyUnion ts) = TyUnion (Set.fromList $ normtypes $ Set.toList ts)
    normtype TyNil = error "missing type annotation"
normalize _ = error "can only normalize quantified types"

-- | Extend type environment
extend :: TypeEnv -> (Var, Type) -> TypeEnv
extend env (v, t) = Map.insert v t env

inTypeEnv :: (Var, Type) -> TypeConstrainT a -> TypeConstrainT a
inTypeEnv (v, t) = local (\e -> Map.delete v e `extend` (v, t))

-- | Lookup type in the environment
checkTy :: Var -> TypeConstrain
checkTy x = do
  env <- ask
  case Map.lookup x env of
    Nothing -> throwError $ IUnboundVariable x
    Just t -> case t of
      TyQuant {} -> instantiate t
      _ -> pure t

freshIfNil :: Type -> TypeConstrain
freshIfNil t = case t of
  TyNil -> fresh
  _ -> pure t

fresh :: TypeConstrain
fresh = do
  s <- get
  let tv = mkTv $ letters !! count s
  put s {count = count s + 1}
  isStale <- asks (isIn tv)
  if isStale
    then fresh
    else pure tv
  where
    isIn ty = elem ty . map snd . Map.toList

tyOp op = case op of S.Add -> tyInt; S.Sub -> tyInt; S.Mul -> tyInt; _ -> tyBool

constrainWith e v t = inTypeEnv (v, t) (constrain e)

instance Inferrable Type S.Expr ConstraintState where
  inferIn e = infer' e initConstrain

  infer = inferIn mkCEnv

  constraintsIn e = constraintsIn' e initConstrain

  constrain b = do
    (a, cs) <- constrain' b
    case runUnify cs of
      Left e -> throwError $ IUnificationError b e
      Right s -> pure (substitute s a, cs)

  constrain' expr = case expr of
    S.ELit (S.LInt _) -> return (tyInt, [])
    S.ELit (S.LBool _) -> return (tyBool, [])
    S.EVar v -> do
      t <- checkTy v
      return (t, [])
    S.ELam (S.Binder v t) e -> do
      t' <- freshIfNil t
      (t'', cs) <- constrainWith e v t'
      return (t' `TyFun` t'', cs)
    S.EApp e0 e1 -> do
      (t0, c0) <- constrain e0
      (t1, c1) <- constrain e1
      tv <- fresh
      return (tv, c0 ++ c1 ++ [(t0, t1 `TyFun` tv)])
    S.EBinOp op e0 e1 -> do
      (t0, c0) <- constrain e0
      (t1, c1) <- constrain e1
      tv <- fresh
      return (tv, c0 ++ c1 ++ [(t0, t1), (tyOp op, tv)])
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
    S.EWildcard -> do
      tv <- fresh
      return (tv, [])
    S.ETyCase e cases -> do
      tv <- fresh
      -- needs to generalize to exprs with vars
      eTs <- mapM (\(S.Binder (S.EVar v) t, e) -> constrainWith e v t) cases
      (et, eCs) <- constrain e

      let (tEs, cEs) = unzip eTs
      let ts = [t | (S.Binder _ t) <- map fst cases]
      let cs =
            [ [(et, TyUnion (Set.fromList ts))],
              concat cEs,
              eCs,
              [(tv, t) | t <- tEs]
            ]

      return (tv, concat cs)
    S.ETree t -> constrain $ S.mkChurchTree t
    _ -> throwError $ IUnconstrainable expr

-------------------------------------------------------------------------------
-- Unification
-------------------------------------------------------------------------------

instance Unifiable Type where
  unify t0 t1 =
    let some t ts = or (unifiable t <$> Set.toList ts)
     in case (t0, t1) of
          _ | t0 == t1 -> pure mempty
          (TyVar v, t) -> pure $ mkEnv v t
          (t, TyVar v) -> pure $ mkEnv v t
          (TyFun t0 t1, TyFun t0' t1') -> do
            u0 <- unify t0 t0'
            u1 <- unify t1 t1'
            pure (u0 <*> u1)
          (TyUnion ts, t) | some t ts -> pure mempty
          (t, TyUnion ts) | some t ts -> pure mempty
          _ -> throwError $ NotUnifiable t0 t1

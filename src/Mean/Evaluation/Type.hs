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
  )
import Data.Either (isRight)
import Data.List (foldl', nub)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import qualified Data.Set as Set
import Debug.Trace (trace, traceM)
import qualified Mean.Syntax.Surface as S
import Mean.Syntax.Type
import Mean.Unification
import Mean.Var
import Mean.Viz
import Prelude hiding ((<*>))

-------------------------------------------------------------------------------
-- Classes
-------------------------------------------------------------------------------

newtype TyEnv = TyEnv {types :: Map.Map Var TyScheme}
  deriving (Eq)

mkTyEnv :: TyEnv
mkTyEnv = TyEnv Map.empty

extend :: TyEnv -> (Var, TyScheme) -> TyEnv
extend env (v, t) = env {types = Map.insert v t (types env)}

remove :: TyEnv -> Var -> TyEnv
remove (TyEnv env) var = TyEnv (Map.delete var env)

merge :: TyEnv -> TyEnv -> TyEnv
merge (TyEnv a) (TyEnv b) = TyEnv (Map.union a b)

toList :: TyEnv -> [(Var, TyScheme)]
toList (TyEnv env) = Map.toList env

fromList :: [(Var, TyScheme)] -> TyEnv
fromList = TyEnv . Map.fromList

isIn :: TyScheme -> TyEnv -> Bool
isIn ty = elem ty . map snd . toList

instance Semigroup TyEnv where
  (<>) = merge

instance Monoid TyEnv where
  mempty = mkTyEnv

-- | Constraint generation monad
type Constrain a =
  ( ReaderT
      TyEnv -- Typing environment
      ( StateT -- Infe state
          ConstraintState
          ( Except -- Infe errors
              TypeError
          )
      )
      a -- Result
  )

letters :: [String]
letters = [1 ..] >>= flip replicateM ['A' .. 'Z']

-- | Constraint generation state
newtype ConstraintState = CState {count :: Int}

-- | Initial constraint generation state
initConstrain :: ConstraintState
initConstrain = CState {count = 0}

class Inferrable a where
  constrain :: a -> Constrain (Type, [Constraint Type])
  infer' :: TyEnv -> a -> Either TypeError TyScheme
  infer :: a -> Either TypeError TyScheme

instance Substitutable Type Type where
  substitute t0 t1 = case (t0, t1) of
    (_, TyCon {}) -> t1
    (e, TyVar a) -> Map.findWithDefault t1 a e
    (s, TyFun t0' t1') -> let sub' = substitute s in sub' t0' `TyFun` sub' t1'
    (s, TyUnion ts) -> TyUnion $ Set.fromList (substitute s <$> Set.toList ts)

instance FV Type where
  fv t = case t of
    TyCon {} -> Set.empty
    TyVar a -> Set.singleton a
    TyFun t0 t1 -> fv t0 `Set.union` fv t1
    TyUnion ts -> foldMap fv ts

instance Substitutable Type TyScheme where
  substitute e (Forall as t) = Forall as $ substitute s' t
    where
      s' = foldr Map.delete e as

instance FV TyScheme where
  fv (Forall as t) = fv t `Set.difference` Set.fromList as

instance Substitutable Type TyEnv where
  substitute s (TyEnv env) = TyEnv $ Map.map (substitute s) env

instance FV TyEnv where
  fv (TyEnv env) = fv $ Map.elems env

data TypeError
  = UnboundVariable Var
  | TyUnificationError (UnificationError Type)
  | UntypableExpr S.Expr
  deriving (Eq, Show)

-------------------------------------------------------------------------------
-- Inference
-------------------------------------------------------------------------------

-- | Canonicalize and return the polymorphic toplevel type.
closeOver :: Type -> TyScheme
closeOver = normalize . generalize mkTyEnv

isTv t = case t of (Forall _ (TyVar Var {})) -> True; _ -> False

-- | Extend type environment
inTyEnv :: (Var, TyScheme) -> Constrain a -> Constrain a
inTyEnv (v, t) m = do
  local (\e -> remove e v `extend` (v, t)) m

-- | Lookup type in the environment
checkTy :: Var -> Constrain Type
checkTy x = do
  (TyEnv env) <- ask
  case Map.lookup x env of
    Nothing -> throwError $ UnboundVariable x
    Just s -> instantiate s

isInTyEnv :: TyScheme -> Constrain Bool
isInTyEnv ty = asks (isIn ty)

freshIfNil :: Type -> Constrain Type
freshIfNil ty = case ty of
  TyNil -> fresh
  _ -> pure ty

instantiate :: TyScheme -> Constrain Type
instantiate (Forall as t) = do
  as' <- mapM (const fresh) as
  let e = Map.fromList $ zip as as'
  return $ substitute e t

generalize :: TyEnv -> Type -> TyScheme
generalize env t = Forall as t
  where
    as = Set.toList $ fv t `Set.difference` fv env

fresh :: Constrain Type
fresh = do
  s <- get
  let tv = mkTv $ letters !! count s
  put s {count = count s + 1}
  isStale <- isInTyEnv $ Forall [] tv
  if isStale
    then fresh
    else pure tv

tyOp op = case op of S.Add -> tyInt; S.Sub -> tyInt; S.Mul -> tyInt; _ -> tyBool

constrainWith e v t = inTyEnv (v, Forall [] t) (constrain e)

instance Inferrable S.Expr where
  infer = infer' mkTyEnv

  infer' env expr = case runConstrain env (constrain expr) of
    Left err -> Left err
    Right (ty, cs) -> case runUnify cs of
      Left err -> Left $ TyUnificationError err
      Right sub -> Right $ closeOver $ substitute sub ty

  constrain expr = case expr of
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
    _ -> throwError $ UntypableExpr expr

normalize :: TyScheme -> TyScheme
normalize (Forall _ body) = Forall (map snd ord) (normtype body)
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

-------------------------------------------------------------------------------
-- Entrypoints
-------------------------------------------------------------------------------

-- | Run the constraint generation monad
runConstrain :: TyEnv -> Constrain (Type, [Constraint Type]) -> Either TypeError (Type, [Constraint Type])
runConstrain env m = runExcept $ evalStateT (runReaderT m env) initConstrain

-- | Return the internal constraints used in solving for the type of an expression
constraintsOnExpr' :: TyEnv -> S.Expr -> Either TypeError ([Constraint Type], Env Type, Type, TyScheme)
constraintsOnExpr' env ex = case runConstrain env (constrain ex) of
  Left err -> Left err
  Right (ty, cs) -> case runUnify cs of
    Left err -> Left $ TyUnificationError err
    Right sub -> Right (cs, sub, ty, sc)
      where
        sc = closeOver $ substitute sub ty

constraintsOnExpr = constraintsOnExpr' mkTyEnv

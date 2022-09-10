{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}

module Mean.Type where

import Data.Monoid

import Control.Monad (replicateM)
import Control.Monad.Except
    ( replicateM,
      MonadError(throwError),
      ExceptT,
      runExcept,
      runExceptT,
      Except )
import Control.Monad.Identity ( Identity(runIdentity) )
import Control.Monad.Reader
    ( MonadReader(ask, local), ReaderT(runReaderT), asks )
import Control.Monad.State
    ( MonadState(put, get), StateT, evalStateT )
import Data.Either (partitionEithers)
import Data.List (nub)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace (traceM)
import qualified Mean.Core as S

letters :: [String]
letters = [1 ..] >>= flip replicateM ['A' .. 'Z']

-------------------------------------------------------------------------------
-- Classes
-------------------------------------------------------------------------------

newtype TyEnv = TyEnv {types :: Map.Map S.Name S.TyScheme}
  deriving (Eq)

empty :: TyEnv
empty = TyEnv Map.empty

extend :: TyEnv -> (S.Name, S.TyScheme) -> TyEnv
extend env (x, s) = env {types = Map.insert x s (types env)}

remove :: TyEnv -> S.Name -> TyEnv
remove (TyEnv env) var = TyEnv (Map.delete var env)

merge :: TyEnv -> TyEnv -> TyEnv
merge (TyEnv a) (TyEnv b) = TyEnv (Map.union a b)

toList :: TyEnv -> [(S.Name, S.TyScheme)]
toList (TyEnv env) = Map.toList env

isIn :: S.TyScheme -> TyEnv -> Bool
isIn ty = elem ty . map snd . toList

instance Semigroup TyEnv where
  (<>) = merge

instance Monoid TyEnv where
  mempty = empty

-- | Inference monad
type Infer a =
  ( ReaderT
      TyEnv -- Typing environment
      ( StateT -- Inference state
          InferState
          ( Except -- Inference errors
              TypeError
          )
      )
      a -- Result
  )

-- | Inference state
newtype InferState = InferState {count :: Int}

-- | Initial inference state
initInfer :: InferState
initInfer = InferState {count = 0}

type Constraint = (S.Type, S.Type)

type Unifier = (Subst, [Constraint])

-- | Constraint solver monad
type Solve a = ExceptT TypeError Identity a

newtype Subst = Subst (Map.Map S.TyVar S.Type)
  deriving (Eq, Ord, Semigroup, Monoid, Show)

class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> Set.Set S.TyVar

instance Substitutable S.Type where
  apply _ (S.TyCon a) = S.TyCon a
  apply (Subst s) t@(S.TyVar a) = Map.findWithDefault t a s
  apply s (t1 `S.TyFun` t2) = apply s t1 `S.TyFun` apply s t2

  ftv S.TyCon {} = Set.empty
  ftv (S.TyVar a) = Set.singleton a
  ftv (t1 `S.TyFun` t2) = ftv t1 `Set.union` ftv t2

instance Substitutable S.TyScheme where
  apply (Subst s) (S.Forall as t) = S.Forall as $ apply s' t
    where
      s' = Subst $ foldr Map.delete s as
  ftv (S.Forall as t) = ftv t `Set.difference` Set.fromList as

instance Substitutable Constraint where
  apply s (t1, t2) = (apply s t1, apply s t2)
  ftv (t1, t2) = ftv t1 `Set.union` ftv t2

instance Substitutable a => Substitutable [a] where
  apply = map . apply
  ftv = foldr (Set.union . ftv) Set.empty

instance Substitutable TyEnv where
  apply s (TyEnv env) = TyEnv $ Map.map (apply s) env
  ftv (TyEnv env) = ftv $ Map.elems env

data TypeError
  = UnificationFail S.Type S.Type
  | InfiniteType S.TyVar S.Type
  | UnboundVariable String
  | UnificationMismatch [S.Type] [S.Type]
  deriving (Eq)

instance Show TypeError where
  show (UnificationFail a b) =
    concat ["cannot unify types: \n\t", show a, "\nwith \n\t", show b]
  show (InfiniteType (S.TV a) b) =
    concat ["cannot construct the infinite type: ", a, " = ", show b]
  show (UnboundVariable a) = "not in scope: " ++ a

-------------------------------------------------------------------------------
-- Inference
-------------------------------------------------------------------------------

-- | Canonicalize and return the polymorphic toplevel type.
closeOver :: S.Type -> S.TyScheme
closeOver = normalize . generalize empty

-- | Extend type environment
inTyEnv :: (S.Name, S.TyScheme) -> Infer a -> Infer a
inTyEnv (x, sc) m = do
  let scope e = remove e x `extend` (x, sc)
  local scope m

-- | Lookup type in the environment
lookupTyEnv :: S.Name -> Infer S.Type
lookupTyEnv x = do
  (TyEnv env) <- ask
  case Map.lookup x env of
    Nothing -> throwError $ UnboundVariable x
    Just s -> do
      instantiate s

isInTyEnv :: S.TyScheme -> Infer Bool
isInTyEnv ty = asks (isIn ty)

fresh :: Infer S.Type
fresh = do
  s <- get
  put s {count = count s + 1}
  freshTyVar s 0
  where
    freshTyVar :: InferState -> Int -> Infer S.Type
    freshTyVar s cnt = do
      let tyVar = S.mkTv $ letters !! (count s + cnt)
      isStale <- isInTyEnv $ S.Forall [] tyVar
      if isStale
        then freshTyVar s (cnt + 1)
        else pure tyVar

freshIfNil :: S.Type -> Infer S.Type
freshIfNil ty = case ty of
  S.TyNil -> fresh
  _ -> pure ty

instantiate :: S.TyScheme -> Infer S.Type
instantiate (S.Forall as t) = do
  as' <- mapM (const fresh) as
  let s = Subst $ Map.fromList $ zip as as'
  return $ apply s t

generalize :: TyEnv -> S.Type -> S.TyScheme
generalize env t = S.Forall as t
  where
    as = Set.toList $ ftv t `Set.difference` ftv env

infer :: S.CoreExpr -> Infer (S.Type, [Constraint])
infer expr = case expr of
  S.CLit (S.LInt _) -> return (S.tyInt, [])
  S.CLit (S.LBool _) -> return (S.tyBool, [])
  S.CVar (S.Var _ v) -> do
    t <- lookupTyEnv v
    return (t, [])
  Lam (S.Binder (S.Var _ v) t) e -> do
    t' <- freshIfNil t
    (t'', cs) <- inTyEnv (v, S.Forall [] t') (infer e)
    return (t' `S.TyFun` t'', cs)
  App e0 e1 -> do
    (t0, c0) <- infer e0
    (t1, c1) <- infer e1
    tv <- fresh
    return (tv, c0 ++ c1 ++ [(t0, t1 `S.TyFun` tv)])

normalize :: S.TyScheme -> S.TyScheme
normalize (S.Forall _ body) = S.Forall (map snd ord) (normtype body)
  where
    ord = zip (nub $ fv body) (map S.TV letters)

    fv (S.TyVar a) = [a]
    fv (S.TyFun a b) = fv a ++ fv b
    fv (S.TyCon _) = []
    fv S.TyNil = error "missing type annotation"

    normtype (S.TyFun a b) = S.TyFun (normtype a) (normtype b)
    normtype (S.TyCon a) = S.TyCon a
    normtype (S.TyVar a) =
      case Prelude.lookup a ord of
        Just x -> S.TyVar x
        Nothing -> error "type variable not in signature"
    normtype S.TyNil = error "missing type annotation"

-------------------------------------------------------------------------------
-- Constraint Solver
-------------------------------------------------------------------------------

-- | The empty substitution
emptySubst :: Subst
emptySubst = mempty

-- | Compose substitutions
compose :: Subst -> Subst -> Subst
(Subst s1) `compose` (Subst s2) = Subst $ Map.map (apply (Subst s1)) s2 `Map.union` s1

-- | Run the constraint solver
runSolve :: [Constraint] -> Either TypeError Subst
runSolve cs = runIdentity $ runExceptT $ solver st
  where
    st = (emptySubst, cs)

unifyMany :: [S.Type] -> [S.Type] -> Solve Subst
unifyMany [] [] = return emptySubst
unifyMany (t1 : ts1) (t2 : ts2) =
  do
    su1 <- unifies t1 t2
    su2 <- unifyMany (apply su1 ts1) (apply su1 ts2)
    return (su2 `compose` su1)
unifyMany t1 t2 = throwError $ UnificationMismatch t1 t2

unifies :: S.Type -> S.Type -> Solve Subst
unifies t1 t2 | t1 == t2 = return emptySubst
unifies (S.TyVar v) t = v `bind` t
unifies t (S.TyVar v) = v `bind` t
unifies (S.TyFun t1 t2) (S.TyFun t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies t1 t2 = throwError $ UnificationFail t1 t2

-- Unification solver
solver :: Unifier -> Solve Subst
solver (su, cs) =
  case cs of
    [] -> return su
    ((t1, t2) : cs0) -> do
      su1 <- unifies t1 t2
      solver (su1 `compose` su, apply su1 cs0)

bind :: S.TyVar -> S.Type -> Solve Subst
bind a t
  | t == S.TyVar a = return emptySubst
  | occursCheck a t = throwError $ InfiniteType a t
  | otherwise = return (Subst $ Map.singleton a t)

occursCheck :: Substitutable a => S.TyVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

-------------------------------------------------------------------------------
-- Entrypoints
-------------------------------------------------------------------------------

-- | Run the inference monad
runInfer :: TyEnv -> Infer (S.Type, [Constraint]) -> Either TypeError (S.Type, [Constraint])
runInfer env m = runExcept $ evalStateT (runReaderT m env) initInfer

type TCList = [Either TypeError (S.Type, [Constraint])]

inferMany :: TyEnv -> [S.CoreExpr] -> Either TypeError [(S.Type, [Constraint])]
inferMany env exprs = case partitionEithers $ foldl go [] exprs of
  (err : _, _) -> Left err
  (_, tcs) -> Right tcs
  where
    go :: TCList -> S.CoreExpr -> TCList
    go tcs expr = runInfer env (infer expr) : tcs

-- | Solve for the toplevel type of an expression in a given environment
inferExpr :: TyEnv -> S.CoreExpr -> Either TypeError S.TyScheme
inferExpr env ex = do
  --  ("inferring " ++ show ex ++ " with ... " ++ "[" ++ show env ++ "]")
  case runInfer env (infer ex) of
    Left err -> Left err
    Right (ty, cs) -> case runSolve cs of
      Left err -> Left err
      Right subst -> Right $ closeOver $ apply subst ty

-- | Infer declaration types, accumulating a type env.
inferModule :: TyEnv -> [(String, S.CoreExpr)] -> Either TypeError TyEnv
inferModule env [] = Right env
inferModule env ((name, ex) : xs) = case inferExpr env ex of
  Left err -> Left err
  Right ty -> inferModule (extend env (name, ty)) xs

-- | Return the internal constraints used in solving for the type of an expression
constraintsOnExpr :: TyEnv -> S.CoreExpr -> Either TypeError ([Constraint], Subst, S.Type, S.TyScheme)
constraintsOnExpr env ex = case runInfer env (infer ex) of
  Left err -> Left err
  Right (ty, cs) -> case runSolve cs of
    Left err -> Left err
    Right subst -> Right (cs, subst, ty, sc)
      where
        sc = closeOver $ apply subst ty

unifiable :: S.Type -> S.Type -> Bool
unifiable t0 t1 = case unify of
  Left {} -> False
  Right {} -> True
  where
    unify = runIdentity $ runExceptT $ unifies t0 t1

t0 <=> t1 = unifiable t0 t1

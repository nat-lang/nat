{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}

module Mean.Evaluation.Type where

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
import Data.List (nub, foldl')
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace (traceM)
import Mean.Syntax.Type
import Mean.Syntax.Surface
import Mean.Viz

letters :: [String]
letters = [1 ..] >>= flip replicateM ['A' .. 'Z']

-------------------------------------------------------------------------------
-- Classes
-------------------------------------------------------------------------------

newtype TyEnv = TyEnv {types :: Map.Map Name TyScheme}
  deriving (Eq)

empty :: TyEnv
empty = TyEnv Map.empty

extend :: TyEnv -> (Name, TyScheme) -> TyEnv
extend env (x, s) = env {types = Map.insert x s (types env)}

remove :: TyEnv -> Name -> TyEnv
remove (TyEnv env) var = TyEnv (Map.delete var env)

merge :: TyEnv -> TyEnv -> TyEnv
merge (TyEnv a) (TyEnv b) = TyEnv (Map.union a b)

toList :: TyEnv -> [(Name, TyScheme)]
toList (TyEnv env) = Map.toList env

isIn :: TyScheme -> TyEnv -> Bool
isIn ty = elem ty . map snd . toList

mkEnv :: [(Name, TyScheme)] -> TyEnv
mkEnv = foldl' extend empty

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

type Constraint = (Type, Type)

type Unifier = (Subst, [Constraint])

-- | Constraint solver monad
type Solve a = ExceptT TypeError Identity a

newtype Subst = Subst (Map.Map TyVar Type)
  deriving (Eq, Ord, Semigroup, Monoid, Show)

class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> Set.Set TyVar

class Inferrable a where
  infer :: a -> Infer (Type, [Constraint])

instance Substitutable Type where
  apply _ (TyCon a) = TyCon a
  apply (Subst s) t@(TyVar a) = Map.findWithDefault t a s
  apply s (t1 `TyFun` t2) = apply s t1 `TyFun` apply s t2

  ftv TyCon {} = Set.empty
  ftv (TyVar a) = Set.singleton a
  ftv (t1 `TyFun` t2) = ftv t1 `Set.union` ftv t2

instance Substitutable TyScheme where
  apply (Subst s) (Forall as t) = Forall as $ apply s' t
    where
      s' = Subst $ foldr Map.delete s as
  ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

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
  = UnificationFail Type Type
  | InfiniteType TyVar Type
  | UnboundVariable String
  | UnificationMismatch [Type] [Type]
  deriving (Eq)

instance Show TypeError where
  show (UnificationFail a b) =
    concat ["cannot unify types: \n\t", show a, "\nwith \n\t", show b]
  show (InfiniteType (TV a) b) =
    concat ["cannot construct the infinite type: ", a, " = ", show b]
  show (UnboundVariable a) = "not in scope: " ++ a

-------------------------------------------------------------------------------
-- Inference
-------------------------------------------------------------------------------

-- | Canonicalize and return the polymorphic toplevel type.
closeOver :: Type -> TyScheme
closeOver = normalize . generalize empty

-- | Extend type environment
inTyEnv :: (Name, TyScheme) -> Infer a -> Infer a
inTyEnv (x, sc) m = do
  let scope e = remove e x `extend` (x, sc)
  local scope m

-- | Lookup type in the environment
lookupTyEnv :: Name -> Infer Type
lookupTyEnv x = do
  (TyEnv env) <- ask
  case Map.lookup x env of
    Nothing -> throwError $ UnboundVariable x
    Just s -> instantiate s

isInTyEnv :: TyScheme -> Infer Bool
isInTyEnv ty = asks (isIn ty)

fresh :: Infer Type
fresh = do
  s <- get
  put s {count = count s + 1}
  freshTyVar s 0
  where
    freshTyVar :: InferState -> Int -> Infer Type
    freshTyVar s cnt = do
      let tyVar = mkTv $ letters !! (count s + cnt)
      isStale <- isInTyEnv $ Forall [] tyVar
      if isStale
        then freshTyVar s (cnt + 1)
        else pure tyVar

freshIfNil :: Type -> Infer Type
freshIfNil ty = case ty of
  TyNil -> fresh
  _ -> pure ty

instantiate :: TyScheme -> Infer Type
instantiate (Forall as t) = do
  as' <- mapM (const fresh) as
  let s = Subst $ Map.fromList $ zip as as'
  return $ apply s t

generalize :: TyEnv -> Type -> TyScheme
generalize env t = Forall as t
  where
    as = Set.toList $ ftv t `Set.difference` ftv env

tyOp op = case op of Add -> tyInt; Sub -> tyInt; Mul -> tyInt; _ -> tyBool

instance Inferrable Expr where
  infer expr = case expr of
    ELit (LInt _) -> return (tyInt, [])
    ELit (LBool _) -> return (tyBool, [])
    EVar (Var _ v) -> do
      t <- lookupTyEnv v
      return (t, [])
    ELam (Binder (Var _ v) t) e -> do
      t' <- freshIfNil t
      (t'', cs) <- inTyEnv (v, Forall [] t') (infer e)
      return (t' `TyFun` t'', cs)
    EApp e0 e1 -> do
      (t0, c0) <- infer e0
      (t1, c1) <- infer e1
      tv <- fresh
      return (tv, c0 ++ c1 ++ [(t0, t1 `TyFun` tv)])
    EBinOp op e0 e1 -> do
      (t0, c0) <- infer e0
      (t1, c1) <- infer e1
      tv <- fresh
      return (tyOp op, c0 ++ c1 ++ [(t0, tv), (t1, tv)])
    ECond x y z -> do
      (tX, cX) <- infer x
      (tY, cY) <- infer y
      (tZ, cZ) <- infer z
      tv <- fresh
      return (tv, cX ++ cY ++ cZ ++ [(tX, tyBool), (tY, tv), (tZ, tv)])
    -- ETyCase (e,ty) cs -> do
    --   tv <- fresh

normalize :: TyScheme -> TyScheme
normalize (Forall _ body) = Forall (map snd ord) (normtype body)
  where
    ord = zip (nub $ fv body) (map TV letters)

    fv (TyVar a) = [a]
    fv (TyFun a b) = fv a ++ fv b
    fv (TyCon _) = []
    fv TyNil = error "missing type annotation"

    normtype (TyFun a b) = TyFun (normtype a) (normtype b)
    normtype (TyCon a) = TyCon a
    normtype (TyVar a) =
      case Prelude.lookup a ord of
        Just x -> TyVar x
        Nothing -> error "type variable not in signature"
    normtype TyNil = error "missing type annotation"

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

unifyMany :: [Type] -> [Type] -> Solve Subst
unifyMany [] [] = return emptySubst
unifyMany (t1 : ts1) (t2 : ts2) =
  do
    su1 <- unifies t1 t2
    su2 <- unifyMany (apply su1 ts1) (apply su1 ts2)
    return (su2 `compose` su1)
unifyMany t1 t2 = throwError $ UnificationMismatch t1 t2

unifies :: Type -> Type -> Solve Subst
unifies t1 t2 | t1 == t2 = return emptySubst
unifies (TyVar v) t = v `bind` t
unifies t (TyVar v) = v `bind` t
unifies (TyFun t1 t2) (TyFun t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies t1 t2 = throwError $ UnificationFail t1 t2

-- Unification solver
solver :: Unifier -> Solve Subst
solver (su, cs) =
  case cs of
    [] -> return su
    ((t1, t2) : cs0) -> do
      su1 <- unifies t1 t2
      solver (su1 `compose` su, apply su1 cs0)

bind :: TyVar -> Type -> Solve Subst
bind a t
  | t == TyVar a = return emptySubst
  -- | occursCheck a t = throwError $ InfiniteType a t
  | otherwise = return (Subst $ Map.singleton a t)

occursCheck :: Substitutable a => TyVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

-------------------------------------------------------------------------------
-- Entrypoints
-------------------------------------------------------------------------------

-- | Run the inference monad
runInfer :: TyEnv -> Infer (Type, [Constraint]) -> Either TypeError (Type, [Constraint])
runInfer env m = runExcept $ evalStateT (runReaderT m env) initInfer

type TCList = [Either TypeError (Type, [Constraint])]

inferMany :: TyEnv -> [Expr] -> Either TypeError [(Type, [Constraint])]
inferMany env exprs = case partitionEithers $ foldl go [] exprs of
  (err : _, _) -> Left err
  (_, tcs) -> Right tcs
  where
    go :: TCList -> Expr -> TCList
    go tcs expr = runInfer env (infer expr) : tcs

-- | Solve for the toplevel type of an expression in a given environment
inferExpr :: TyEnv -> Expr -> Either TypeError TyScheme
inferExpr env ex = do
  --  ("inferring " ++ show ex ++ " with ... " ++ "[" ++ show env ++ "]")
  case runInfer env (infer ex) of
    Left err -> Left err
    Right (ty, cs) -> case runSolve cs of
      Left err -> Left err
      Right subst -> Right $ closeOver $ apply subst ty

-- | Infer declaration types, accumulating a type env.
inferModule :: TyEnv -> [(String, Expr)] -> Either TypeError TyEnv
inferModule env [] = Right env
inferModule env ((name, ex) : xs) = case inferExpr env ex of
  Left err -> Left err
  Right ty -> inferModule (extend env (name, ty)) xs

-- | Return the internal constraints used in solving for the type of an expression
constraintsOnExpr :: TyEnv -> Expr -> Either TypeError ([Constraint], Subst, Type, TyScheme)
constraintsOnExpr env ex = case runInfer env (infer ex) of
  Left err -> Left err
  Right (ty, cs) -> case runSolve cs of
    Left err -> Left err
    Right subst -> Right (cs, subst, ty, sc)
      where
        sc = closeOver $ apply subst ty

unifiable :: Type -> Type -> Bool
unifiable t0 t1 = case unify of
  Left {} -> False
  Right {} -> True
  where
    unify = runIdentity $ runExceptT $ unifies t0 t1

(<=>) = unifiable

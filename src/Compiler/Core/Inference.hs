{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Compiler.Core.Inference
  ( Constraint,
    TypeError (..),
    Subst (..),
    inferTop,
    inferExpr,
    constraintsExpr,
    unifiable,
  )
where

import Compiler.Core.Fresh (letters)
import Compiler.Core.Pretty
import qualified Compiler.Core.Syntax as Syn
import Compiler.Core.TypeEnv
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Data.Either (partitionEithers)
import Data.List (nub)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace (traceM)

-------------------------------------------------------------------------------
-- Classes
-------------------------------------------------------------------------------

-- | Inference monad
type Infer a =
  ( ReaderT
      Env -- Typing environment
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

type Constraint = (Syn.Type, Syn.Type)

type Unifier = (Subst, [Constraint])

-- | Constraint solver monad
type Solve a = ExceptT TypeError Identity a

newtype Subst = Subst (Map.Map Syn.TyVar Syn.Type)
  deriving (Eq, Ord, Semigroup, Monoid)

class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> Set.Set Syn.TyVar

instance Substitutable Syn.Type where
  apply _ (Syn.TyCon a) = Syn.TyCon a
  apply (Subst s) t@(Syn.TyVar a) = Map.findWithDefault t a s
  apply s (t1 `Syn.TyFun` t2) = apply s t1 `Syn.TyFun` apply s t2

  ftv Syn.TyCon {} = Set.empty
  ftv (Syn.TyVar a) = Set.singleton a
  ftv (t1 `Syn.TyFun` t2) = ftv t1 `Set.union` ftv t2

instance Substitutable Syn.TyScheme where
  apply (Subst s) (Syn.Forall as t) = Syn.Forall as $ apply s' t
    where
      s' = Subst $ foldr Map.delete s as
  ftv (Syn.Forall as t) = ftv t `Set.difference` Set.fromList as

instance Substitutable Constraint where
  apply s (t1, t2) = (apply s t1, apply s t2)
  ftv (t1, t2) = ftv t1 `Set.union` ftv t2

instance Substitutable a => Substitutable [a] where
  apply = map . apply
  ftv = foldr (Set.union . ftv) Set.empty

instance Substitutable Env where
  apply s (TypeEnv env) = TypeEnv $ Map.map (apply s) env
  ftv (TypeEnv env) = ftv $ Map.elems env

data TypeError
  = UnificationFail Syn.Type Syn.Type
  | InfiniteType Syn.TyVar Syn.Type
  | UnboundVariable String
  | Ambigious [Constraint]
  | UnificationMismatch [Syn.Type] [Syn.Type]

instance Show TypeError where
  show (UnificationFail a b) =
    concat ["cannot unify types: \n\t", pptype a, "\nwith \n\t", pptype b]
  show (InfiniteType (Syn.TV a) b) =
    concat ["cannot construct the infinite type: ", a, " = ", pptype b]
  show (Ambigious cs) =
    concat ["cannot match expected type: '" ++ pptype a ++ "' with actual type: '" ++ pptype b ++ "'\n" | (a, b) <- cs]
  show (UnboundVariable a) = "not in scope: " ++ a

-------------------------------------------------------------------------------
-- Inference
-------------------------------------------------------------------------------

-- | Canonicalize and return the polymorphic toplevel type.
closeOver :: Syn.Type -> Syn.TyScheme
closeOver = normalize . generalize empty

-- | Extend type environment
inEnv :: (Syn.Name, Syn.TyScheme) -> Infer a -> Infer a
inEnv (x, sc) m = do
  let scope e = remove e x `extend` (x, sc)
  local scope m

-- | Lookup type in the environment
lookupEnv :: Syn.Name -> Infer Syn.Type
lookupEnv x = do
  (TypeEnv env) <- ask
  case Map.lookup x env of
    Nothing -> throwError $ UnboundVariable x
    Just s -> do
      instantiate s

isInEnv :: Syn.TyScheme -> Infer Bool
isInEnv ty = asks (isIn ty)

fresh :: Infer Syn.Type
fresh = do
  s <- get
  put s {count = count s + 1}
  freshTyVar s 0
  where
    freshTyVar :: InferState -> Int -> Infer Syn.Type
    freshTyVar s cnt = do
      let tyVar = Syn.TyVar $ Syn.TV $ letters !! (count s + cnt)
      isStale <- isInEnv $ Syn.mkTyScheme tyVar
      if isStale
        then freshTyVar s (cnt + 1)
        else pure tyVar

freshIfNil :: Syn.Type -> Infer Syn.Type
freshIfNil ty = case ty of
  Syn.TyNil -> fresh
  _ -> pure ty

instantiate :: Syn.TyScheme -> Infer Syn.Type
instantiate (Syn.Forall as t) = do
  as' <- mapM (const fresh) as
  let s = Subst $ Map.fromList $ zip as as'
  return $ apply s t

generalize :: Env -> Syn.Type -> Syn.TyScheme
generalize env t = Syn.Forall as t
  where
    as = Set.toList $ ftv t `Set.difference` ftv env

infer :: Syn.Expr -> Infer (Syn.Type, [Constraint])
infer expr = case expr of
  Syn.ELit (Syn.LInt _) -> return (Syn.tyInt, [])
  Syn.ELit (Syn.LBool _) -> return (Syn.tyBool, [])
  Syn.EBinder (Syn.Binder n t) -> do
    t' <- freshIfNil t
    pure (t', [])
  Syn.Const c t -> do
    t' <- freshIfNil t
    pure (t', [])
  Syn.Var x -> do
    t <- lookupEnv x
    return (t, [])
  Syn.Lam (Syn.Binder n t) e -> do
    t' <- freshIfNil t
    (t'', cs) <- inEnv (n, Syn.Forall [] t') (infer e)
    return (t' `Syn.TyFun` t'', cs)

  -- fixme
  Syn.Pred n t es -> do
    env <- ask
    t' <- freshIfNil t
    case inferMany env es of
      Left err -> throwError err
      Right infs -> return (t', [])
  -- Right infs -> return (t, concat $ snd $ unzip infs)

  Syn.App e0 e1 -> do
    (t1, c1) <- infer e0
    (t2, c2) <- infer e1
    tv <- fresh
    return (tv, c1 ++ c2 ++ [(t1, t2 `Syn.TyFun` tv)])
  Syn.EComparison c e0 e1 -> do
    (t0, c0) <- infer e0
    (t1, c1) <- infer e1
    tv <- fresh
    let c2 = (tv, Syn.tyBool)

    return (tv, c2 : c0 ++ c1)
  Syn.EUnOp op e -> pure (Syn.tyBool, [])
  Syn.EBinOp op e0 e1 -> do
    (t1, c1) <- infer e0
    (t2, c2) <- infer e1
    tv <- fresh
    let u1 = t1 `Syn.TyFun` (t2 `Syn.TyFun` tv)
        u2 = case op of
          Syn.Conj -> Syn.tyBool `Syn.TyFun` (Syn.tyBool `Syn.TyFun` Syn.tyBool)
          Syn.Disj -> Syn.tyBool `Syn.TyFun` (Syn.tyBool `Syn.TyFun` Syn.tyBool)
          -- otherwise arithmetic ops
          _ -> Syn.tyInt `Syn.TyFun` (Syn.tyInt `Syn.TyFun` Syn.tyInt)

    return (tv, c1 ++ c2 ++ [(u1, u2), (t1, t2)])
  Syn.EQuant q (Syn.Binder n t) e -> do
    t' <- freshIfNil t
    (_, c) <- inEnv (n, Syn.Forall [] t') (infer e)
    return (Syn.tyBool, c)

{-
Let x e0 e1 -> do
  env <- ask
  (t1, c1) <- infer e0
  case runSolve c1 of
      Left err -> throwError err
      Right sub -> do
          let sc = generalize (apply sub env) (apply sub t1)
          (t2, c2) <- inEnv (x, sc) $ local (apply sub) (infer e1)
          return (t2, c1 ++ c2)

Fix e0 -> do
  (t1, c1) <- infer e0
  tv <- fresh
  return (tv, c1 ++ [(tv `Syn.TyFun` tv, t1)])

If cond tr fl -> do
  (t1, c1) <- infer cond
  (t2, c2) <- infer tr
  (t3, c3) <- infer fl
  return (t2, c1 ++ c2 ++ c3 ++ [(t1, Syn.tyBool), (t2, t3)])
-}

normalize :: Syn.TyScheme -> Syn.TyScheme
normalize (Syn.Forall _ body) = Syn.Forall (map snd ord) (normtype body)
  where
    ord = zip (nub $ fv body) (map Syn.TV letters)

    fv (Syn.TyVar a) = [a]
    fv (Syn.TyFun a b) = fv a ++ fv b
    fv (Syn.TyCon _) = []

    normtype (Syn.TyFun a b) = Syn.TyFun (normtype a) (normtype b)
    normtype (Syn.TyCon a) = Syn.TyCon a
    normtype (Syn.TyVar a) =
      case Prelude.lookup a ord of
        Just x -> Syn.TyVar x
        Nothing -> error "type variable not in signature"

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

unifyMany :: [Syn.Type] -> [Syn.Type] -> Solve Subst
unifyMany [] [] = return emptySubst
unifyMany (t1 : ts1) (t2 : ts2) =
  do
    su1 <- unifies t1 t2
    su2 <- unifyMany (apply su1 ts1) (apply su1 ts2)
    return (su2 `compose` su1)
unifyMany t1 t2 = throwError $ UnificationMismatch t1 t2

unifies :: Syn.Type -> Syn.Type -> Solve Subst
unifies t1 t2 | t1 == t2 = return emptySubst
unifies (Syn.TyVar v) t = v `bind` t
unifies t (Syn.TyVar v) = v `bind` t
unifies (Syn.TyFun t1 t2) (Syn.TyFun t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies t1 t2 = throwError $ UnificationFail t1 t2

unifiable :: Syn.Type -> Syn.Type -> Bool
unifiable t0 t1 = case unify of
  Left {} -> False
  Right {} -> True
  where
    unify = runIdentity $ runExceptT $ unifies t0 t1

-- Unification solver
solver :: Unifier -> Solve Subst
solver (su, cs) =
  case cs of
    [] -> return su
    ((t1, t2) : cs0) -> do
      su1 <- unifies t1 t2
      solver (su1 `compose` su, apply su1 cs0)

bind :: Syn.TyVar -> Syn.Type -> Solve Subst
bind a t
  | t == Syn.TyVar a = return emptySubst
  | occursCheck a t = throwError $ InfiniteType a t
  | otherwise = return (Subst $ Map.singleton a t)

occursCheck :: Substitutable a => Syn.TyVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

-------------------------------------------------------------------------------
-- Entrypoints
-------------------------------------------------------------------------------

-- | Run the inference monad
runInfer :: Env -> Infer (Syn.Type, [Constraint]) -> Either TypeError (Syn.Type, [Constraint])
runInfer env m = runExcept $ evalStateT (runReaderT m env) initInfer

type TCList = [Either TypeError (Syn.Type, [Constraint])]

inferMany :: Env -> [Syn.Expr] -> Either TypeError [(Syn.Type, [Constraint])]
inferMany env exprs = case partitionEithers $ foldl go [] exprs of
  (err : _, _) -> Left err
  (_, tcs) -> Right tcs
  where
    go :: TCList -> Syn.Expr -> TCList
    go tcs expr = runInfer env (infer expr) : tcs

-- | Solve for the toplevel type of an expression in a given environment
inferExpr :: Env -> Syn.Expr -> Either TypeError Syn.TyScheme
inferExpr env ex = do
  --  ("inferring " ++ show ex ++ " with ... " ++ "[" ++ show env ++ "]")
  case runInfer env (infer ex) of
    Left err -> Left err
    Right (ty, cs) -> case runSolve cs of
      Left err -> Left err
      Right subst -> Right $ closeOver $ apply subst ty

-- | Infer declaration types, accumulating a type env.
inferTop :: Env -> [(String, Syn.Expr)] -> Either TypeError Env
inferTop env [] = Right env
inferTop env ((name, ex) : xs) = case inferExpr env ex of
  Left err -> Left err
  Right ty -> inferTop (extend env (name, ty)) xs

-- | Return the internal constraints used in solving for the type of an expression
constraintsExpr :: Env -> Syn.Expr -> Either TypeError ([Constraint], Subst, Syn.Type, Syn.TyScheme)
constraintsExpr env ex = case runInfer env (infer ex) of
  Left err -> Left err
  Right (ty, cs) -> case runSolve cs of
    Left err -> Left err
    Right subst -> Right (cs, subst, ty, sc)
      where
        sc = closeOver $ apply subst ty
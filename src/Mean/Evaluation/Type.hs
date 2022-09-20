{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
  )
import Data.Either (isRight)
import Data.List (foldl', nub)
import qualified Data.Map as Map
import Data.Monoid
import qualified Data.Set as Set
import Debug.Trace (traceM)
import qualified Mean.Syntax.Surface as S
import Mean.Syntax.Type
import Mean.Viz

letters :: [String]
letters = [1 ..] >>= flip replicateM ['A' .. 'Z']

-------------------------------------------------------------------------------
-- Classes
-------------------------------------------------------------------------------

newtype TyEnv = TyEnv {types :: Map.Map S.Name TyScheme}
  deriving (Eq)

mkTyEnv :: TyEnv
mkTyEnv = TyEnv Map.empty

extend :: TyEnv -> (S.Name, TyScheme) -> TyEnv
extend env (x, s) = env {types = Map.insert x s (types env)}

remove :: TyEnv -> S.Name -> TyEnv
remove (TyEnv env) var = TyEnv (Map.delete var env)

merge :: TyEnv -> TyEnv -> TyEnv
merge (TyEnv a) (TyEnv b) = TyEnv (Map.union a b)

toList :: TyEnv -> [(S.Name, TyScheme)]
toList (TyEnv env) = Map.toList env

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
          InferState
          ( Except -- Infe errors
              TypeError
          )
      )
      a -- Result
  )

-- | Inference state
newtype InferState = InferState {count :: Int}

-- | Initial inference state
initConstrain :: InferState
initConstrain = InferState {count = 0}

type Constraint = (Type, Type)

-- | Constraint solver monad
type UnifyM a = ExceptT TypeError Identity a

newtype Substitution = Sub (Map.Map TyVar Type) deriving (Eq, Ord, Semigroup, Monoid, Show)

class Substitutable a where
  substitute :: Substitution -> a -> a
  fv :: a -> Set.Set TyVar

class Inferrable a where
  constrain :: a -> Constrain (Type, [Constraint])
  infer' :: TyEnv -> a -> Either TypeError TyScheme
  infer :: a -> Either TypeError TyScheme

class Unifiable a where
  unify :: a -> UnifyM Substitution

instance Substitutable Type where
  substitute _ (TyCon a) = TyCon a
  substitute (Sub s) t@(TyVar a) = Map.findWithDefault t a s
  substitute s (t0 `TyFun` t1) = let sub' = substitute s in sub' t0 `TyFun` sub' t1

  fv TyCon {} = Set.empty
  fv (TyVar a) = Set.singleton a
  fv (t1 `TyFun` t2) = fv t1 `Set.union` fv t2

instance Substitutable TyScheme where
  substitute (Sub s) (Forall as t) = Forall as $ substitute s' t
    where
      s' = Sub $ foldr Map.delete s as
  fv (Forall as t) = fv t `Set.difference` Set.fromList as

instance Substitutable Constraint where
  substitute s (t0, t1) = (substitute s t0, substitute s t1)
  fv (t0, t1) = fv t0 `Set.union` fv t1

instance Substitutable a => Substitutable [a] where
  substitute = map . substitute
  fv = foldr (Set.union . fv) Set.empty

instance Substitutable TyEnv where
  substitute s (TyEnv env) = TyEnv $ Map.map (substitute s) env
  fv (TyEnv env) = fv $ Map.elems env

data TypeError
  = NotUnifiable Type Type
  | InfiniteType TyVar Type
  | UnboundVariable String
  | UnificationMismatch [Type] [Type]
  deriving (Eq)

instance Show TypeError where
  show (NotUnifiable a b) =
    concat ["cannot unify types: \n\t", show a, "\nwith \n\t", show b]
  show (InfiniteType (TV a) b) =
    concat ["cannot construct the infinite type: ", a, " = ", show b]
  show (UnboundVariable a) = "not in scope: " ++ a

-------------------------------------------------------------------------------
-- Inference
-------------------------------------------------------------------------------

-- | Canonicalize and return the polymorphic toplevel type.
closeOver :: Type -> TyScheme
closeOver = normalize . generalize mkTyEnv

-- | Extend type environment
inTyEnv :: (S.Name, TyScheme) -> Constrain a -> Constrain a
inTyEnv (x, sc) m = do
  let scope e = remove e x `extend` (x, sc)
  local scope m

-- | Lookup type in the environment
lookupTyEnv :: S.Name -> Constrain Type
lookupTyEnv x = do
  (TyEnv env) <- ask
  case Map.lookup x env of
    Nothing -> throwError $ UnboundVariable x
    Just s -> instantiate s

isInTyEnv :: TyScheme -> Constrain Bool
isInTyEnv ty = asks (isIn ty)

fresh :: Constrain Type
fresh = do
  s <- get
  put s {count = count s + 1}
  freshTyVar s 0
  where
    freshTyVar :: InferState -> Int -> Constrain Type
    freshTyVar s cnt = do
      let tyVar = mkTv $ letters !! (count s + cnt)
      isStale <- isInTyEnv $ Forall [] tyVar
      if isStale
        then freshTyVar s (cnt + 1)
        else pure tyVar

freshIfNil :: Type -> Constrain Type
freshIfNil ty = case ty of
  TyNil -> fresh
  _ -> pure ty

instantiate :: TyScheme -> Constrain Type
instantiate (Forall as t) = do
  as' <- mapM (const fresh) as
  let s = Sub $ Map.fromList $ zip as as'
  return $ substitute s t

generalize :: TyEnv -> Type -> TyScheme
generalize env t = Forall as t
  where
    as = Set.toList $ fv t `Set.difference` fv env

tyOp op = case op of S.Add -> tyInt; S.Sub -> tyInt; S.Mul -> tyInt; _ -> tyBool

instance Inferrable S.Expr where
  infer = infer' mkTyEnv

  infer' env expr = case runConstrain env (constrain expr) of
    Left err -> Left err
    Right (ty, cs) -> case runUnify cs of
      Left err -> Left err
      Right sub -> Right $ closeOver $ substitute sub ty

  constrain expr = case expr of
    S.ELit (S.LInt _) -> return (tyInt, [])
    S.ELit (S.LBool _) -> return (tyBool, [])
    S.EVar (S.Var _ v) -> do
      t <- lookupTyEnv v
      return (t, [])
    S.ELam (S.Binder (S.Var _ v) t) e -> do
      t' <- freshIfNil t
      (t'', cs) <- inTyEnv (v, Forall [] t') (constrain e)
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
      return (tyOp op, c0 ++ c1 ++ [(t0, tv), (t1, tv)])
    S.ECond x y z -> do
      (tX, cX) <- constrain x
      (tY, cY) <- constrain y
      (tZ, cZ) <- constrain z
      tv <- fresh
      return (tv, cX ++ cY ++ cZ ++ [(tX, tyBool), (tY, tv), (tZ, tv)])
    S.ETyCase e cases ->
      let (ts, es) = unzip cases
       in do
            tv <- fresh
            eTs <- mapM constrain es
            (et, eCs) <- constrain e

            let (tEs, cEs) = unzip eTs
            let cs =
                  concat
                    [ [(et, TyUnion (Set.fromList ts))],
                      concat cEs,
                      eCs,
                      [(tv, t) | t <- tEs]
                    ]
            return (tv, cs)

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
emptySub :: Substitution
emptySub = mempty

-- | Compose substitutions
compose :: Substitution -> Substitution -> Substitution
compose (Sub s0) (Sub s1) = Sub $ Map.map (substitute (Sub s0)) s1 `Map.union` s0

-- | Run the constraint solver
runUnify :: [Constraint] -> Either TypeError Substitution
runUnify = runIdentity . runExceptT . unify

instance Unifiable [(Type, Type)] where
  unify cs = case cs of
    [] -> pure emptySub
    ((t0, t1) : cs') ->
      let sub v t = do
            let sub' = Sub $ Map.singleton v t
            u <- unify $ substitute sub' cs'
            pure $ u `compose` sub'
       in case (t0, t1) of
            _ | t0 == t1 -> unify cs'
            (TyVar v, t) -> sub v t
            (t, TyVar v) -> sub v t
            (TyFun t0 t1, TyFun t0' t1') -> unify (cs' ++ [(t0, t0'), (t1, t1')])
            _ -> throwError $ NotUnifiable t0 t1

-------------------------------------------------------------------------------
-- Entrypoints
-------------------------------------------------------------------------------

-- | Run the constraint generation monad
runConstrain :: TyEnv -> Constrain (Type, [Constraint]) -> Either TypeError (Type, [Constraint])
runConstrain env m = runExcept $ evalStateT (runReaderT m env) initConstrain

-- | Return the internal constraints used in solving for the type of an expression
constraintsOnExpr :: TyEnv -> S.Expr -> Either TypeError ([Constraint], Substitution, Type, TyScheme)
constraintsOnExpr env ex = case runConstrain env (constrain ex) of
  Left err -> Left err
  Right (ty, cs) -> case runUnify cs of
    Left err -> Left err
    Right sub -> Right (cs, sub, ty, sc)
      where
        sc = closeOver $ substitute sub ty

unifiable :: Type -> Type -> Bool
unifiable t0 t1 = isRight $ runIdentity $ runExceptT $ unify [(t0, t1)]

(<=>) = unifiable

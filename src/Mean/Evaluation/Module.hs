{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Mean.Evaluation.Module where

import Control.Monad (MonadPlus (..), foldM, forM, (<=<))
import Control.Monad.Except (Except, liftEither, runExcept, runExceptT, throwError, withExceptT)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (MonadReader (local), ReaderT (runReaderT), ask)
import Control.Monad.State (evalStateT)
import Data.Foldable (Foldable (foldl'))
import Data.Function ((&))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace (trace, traceM)
import Mean.Context
import Mean.Control (foldM1, mapAccumM)
import Mean.Evaluation.Context
import Mean.Evaluation.Surface
import Mean.Evaluation.Type
import Mean.Inference
import Mean.Reduction
import Mean.Syntax.Module
import Mean.Syntax.Surface
import Mean.Syntax.Type
import Mean.Unification
import Mean.Walk (Walkable (walkM))

data ModuleEvalError
  = MExprEvalError ExprEvalError
  | MTypeError (InferenceError Type Expr)
  deriving (Eq, Show)

type ModuleExprReduction = Reduction ModuleExpr ExprEvalError (TypeEnv, ModuleEnv)

type ModuleEnv = Map.Map Var Expr

toExpr = \case MDecl _ e -> e; MLetRec v e -> EFix v e; MExec e -> e

instance Reducible ModuleExpr ModuleExpr ExprEvalError TypeEnv where
  reduce = \case
    MDecl v e -> MDecl v <$> reduce e
    MLetRec v e -> MLetRec v <$> reduce (EFix v e)
    MExec e -> MExec <$> reduce e

instance Contextual ModuleExpr where
  fv = \case
    MDecl _ e -> fv e
    MLetRec v e -> fv e Set.\\ Set.singleton v
    MExec e -> fv e
  bv = \case
    MDecl v _ -> Set.singleton v
    MLetRec v _ -> Set.singleton v
    _ -> Set.empty

instance {-# OVERLAPPING #-} Contextual Module where
  fv mod = fv' mod Set.\\ bv mod
    where
      fv' = foldr (Set.union . fv) Set.empty

instance Substitutable Expr ModuleExpr where
  sub v expr mExpr =
    let sub' = sub v expr
     in case mExpr of
          MDecl v e -> MDecl v (sub' e)
          MLetRec v' e | v /= v' -> MLetRec v' (sub' e)
          MExec e -> MExec (sub' e)
          _ -> mExpr

instance Renamable ModuleExpr where
  rename' vs mExpr = case mExpr of
    MDecl v e -> MDecl v <$> rename' vs e
    MLetRec v e -> MLetRec v <$> rename' vs e
    MExec e -> MExec <$> rename' vs e

instance Renamable Module where
  rename' vs mod = (thdPass <=< sndPass <=< fstPass) mod
    where
      reBndEnv mod = Map.fromList $ [(reset v, EVar v) | v <- Set.toList $ bv mod]

      -- rename topmost let vars
      fstPass :: Module -> RenameM Module
      fstPass mod = forM mod $ \case
        MDecl v e -> MDecl <$> next v <*> pure e
        MLetRec v e -> MLetRec <$> next v <*> pure e
        mExpr -> pure mExpr

      -- update topmost let vars bound in every expr
      sndPass :: Module -> RenameM Module
      sndPass mod = pure $ inEnv (reBndEnv mod) <$> mod

      -- now rename each expr, ignoring the bound let vars
      thdPass :: Module -> RenameM Module
      thdPass mod =
        let rename e = rename' vs e
         in mapM rename mod

runRenameTypes = runRename' . m
  where
    rn = walkETypesM rename
    m mod = forM mod $ \case
      MDecl v e -> MDecl v <$> rn e
      MLetRec v e -> MLetRec v <$> rn e
      MExec e -> MExec <$> rn e

renameMod = runRenameTypes . runRename

toEnv t = \case
  MDecl v _ -> Map.singleton v t
  MLetRec v _ -> Map.singleton v t
  MExec _ -> Map.empty

typeMod :: Module -> Either (InferenceError Type Expr) TypeEnv
typeMod mod = run (foldM typeMod' mkCEnv mod)
  where
    run m = runExcept $ evalStateT (runReaderT m mkCEnv) mkCState
    typeMod' :: TypeEnv -> ModuleExpr -> ConstrainT Type Expr ConstraintState TypeEnv
    typeMod' env mExpr = local (Map.union env) $ do
      (t, env') <- signify (toExpr mExpr)
      pure $ Map.unions [toEnv t mExpr, env', env]

reduceMod :: Module -> TypeEnv -> Either ExprEvalError (ModuleEnv, Module)
reduceMod mod tyEnv = run (mapAccumM accumModM Map.empty mod)
  where
    run m = runIdentity $ runExceptT (runReaderT m tyEnv)
    merge modExpr modEnv = Map.union modEnv $ case modExpr of
      MDecl v e -> Map.singleton v e
      MLetRec v e -> Map.singleton v e
      _ -> Map.empty
    accumModM env expr = do
      expr' <- reduce (inEnv env expr)
      pure (merge expr' env, expr')

eval :: Module -> Either ModuleEvalError Module
eval mod = runExcept $ (reduceMod' <=< typeMod') mod
  where
    mod' = renameMod mod
    typeMod' mod = case typeMod mod of
      Left err -> throwError $ MTypeError err
      Right tyEnv -> pure tyEnv
    reduceMod' tyEnv = case reduceMod mod' tyEnv of
      Left err -> throwError $ MExprEvalError err
      Right (_, mod) -> pure mod
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Nat.Evaluation.Module where

import Control.Monad (foldM, forM, (<=<))
import Control.Monad.Except (Except, liftEither, runExcept, runExceptT, throwError, withExceptT)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (MonadReader (local), ReaderT (runReaderT), ask)
import Control.Monad.State (evalStateT)
import Data.Foldable (Foldable (foldl'))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace (trace, traceM)
import Nat.Context
import Nat.Control (mapAccumM)
import Nat.Evaluation.Context
import Nat.Evaluation.Surface
import Nat.Evaluation.Type
import Nat.Inference
import Nat.Reduction
import Nat.Syntax.Module
import Nat.Syntax.Surface
import Nat.Syntax.Type
import Nat.Unification
import Nat.Walk (Walkable (walkM))

data ModuleEvalError
  = MExprEvalError ExprEvalError
  | MTypeError (InferenceError Type Expr)
  deriving (Eq, Show)

type ModuleExprReduction = Reduction ModuleExpr ExprEvalError (TypeEnv, ModuleEnv)

type ModuleEnv = Map.Map Var Expr

toExpr = \case MDecl _ e -> e; MLetRec v e -> EFix v e; MExec e -> e

instance Reducible ModuleExpr ModuleExpr ExprEvalError TypeEnv where
  reduce = mapM reduce

toEnv t = \case
  MDecl v _ -> Map.singleton v t
  MLetRec v _ -> Map.singleton v t
  MExec _ -> Map.empty

typeMod :: Module -> Int -> Either (InferenceError Type Expr) TypeEnv
typeMod mod = run (foldM typeMod' mkCEnv mod)
  where
    run m s = runExcept $ evalStateT (runReaderT m mkCEnv) s
    typeMod' :: TypeEnv -> ModuleExpr -> ConstrainT Type Expr TypeEnv
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
    (mod', s) = runRename mod
    typeMod' mod = case typeMod mod s of
      Left err -> throwError $ MTypeError err
      Right tyEnv -> pure tyEnv
    reduceMod' tyEnv = case reduceMod mod' tyEnv of
      Left err -> throwError $ MExprEvalError err
      Right (_, mod) -> pure mod
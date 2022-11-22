{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Nat.Evaluation.Module where

import Control.Monad (foldM, forM, (<=<))
import Control.Monad.Except (Except, ExceptT, liftEither, runExcept, runExceptT, throwError, withExceptT)
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

data ModuleEvalError
  = MExprEvalError ExprEvalError
  | MTypeError (InferenceError Type Expr)
  deriving (Eq, Show)

type ModuleEnv = Map.Map Var Expr

type ModuleEvalT a = ExceptT ModuleEvalError Identity a

instance Reducible ModuleExpr ModuleExpr ExprEvalError ExprReductionEnv where
  reduce = mapM reduce

instance Reducible Module Module ExprEvalError ExprReductionEnv where
  reduce mod = do
    (_, mod') <- mapAccumM reduce' Map.empty mod
    return mod'
    where
      reduce' env expr = do
        expr' <- reduce (inEnv env expr)
        pure (merge expr' env, expr')
      merge modExpr modEnv = Map.union modEnv $ case modExpr of
        MDecl v e -> Map.singleton v (toExpr modExpr)
        _ -> Map.empty

toExpr = \case
  -- is the declaration recursive?
  MDecl v e | Set.member v (fv e) -> EFix v e
  MDecl _ e -> e
  MExec e -> e

toEnv t = \case
  MDecl v _ -> Map.singleton v t
  MExec _ -> Map.empty

typeMod' :: Module -> Int -> Either (InferenceError Type Expr) TypeEnv
typeMod' mod = run (foldM signifyIn mkCEnv mod)
  where
    run m s = runExcept $ evalStateT (runReaderT m mkCEnv) s
    signifyIn :: TypeEnv -> ModuleExpr -> ConstrainT Type Expr TypeEnv
    signifyIn env mExpr = local (Map.union env) $ do
      (t, env') <- signify (toExpr mExpr)
      -- traceM
      pure $ Map.unions [toEnv t mExpr, env', env]

typeMod ::
  Int ->
  Module ->
  ModuleEvalT TypeEnv
typeMod s mod = case typeMod' mod s of
  Left err -> throwError $ MTypeError err
  Right tyEnv -> pure tyEnv

reduceMod ::
  Module ->
  TypeEnv ->
  ModuleEvalT Module
reduceMod mod tyEnv = case reduceIn tyEnv mod of
  Left err -> throwError $ MExprEvalError err
  Right mod' -> pure mod'
  where
    reduceIn tyEnv = runReduce' (ExprRedEnv {tyEnv = tyEnv, relEnv = Map.empty})

preproc = runRename . fmap (fmap desugar)

runTypeMod mod = runExcept (typeMod 0 mod)

eval :: Module -> Either ModuleEvalError Module
eval mod = runExcept $ (reduceMod mod' <=< typeMod nameSupply) mod'
  where
    (mod', nameSupply) = preproc mod

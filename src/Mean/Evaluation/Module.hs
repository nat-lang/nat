{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Mean.Evaluation.Module where

import Control.Monad (MonadPlus (..), foldM, (<=<))
import Control.Monad.Except (Except, liftEither, runExcept, runExceptT, throwError, withExceptT)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (ReaderT (runReaderT), ask)
import Control.Monad.State (evalStateT)
import qualified Data.Map as Map
import Debug.Trace (trace)
import Mean.Context
import Mean.Control (foldM1, mapAccumM)
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

toExpr = \case MDecl _ e -> e; MExec e -> e

instance Reducible ModuleExpr ModuleExpr ExprEvalError TypeEnv where
  reduce = \case
    MDecl v e -> MDecl v <$> reduce e
    MExec e -> MExec <$> reduce e

instance Contextual ModuleExpr where
  fv = \case
    MDecl _ e -> fv e
    MExec e -> fv e

instance Substitutable Expr ModuleExpr where
  sub v expr mExpr =
    let sub' = sub v expr
     in case mExpr of
          MDecl v e -> MDecl v (sub' e)
          MExec e -> MExec (sub' e)

instance Renamable ModuleExpr where
  rename mExpr = case mExpr of
    MDecl v e -> MDecl <$> next v <*> rename e
    MExec e -> MExec <$> rename e

merge modExpr modEnv = Map.union modEnv $ case modExpr of
  MDecl v e -> Map.singleton v e
  _ -> Map.empty

renameMod mod = runIdentity $ evalStateT (mapM renameMod' mod) 0
  where
    renameMod' :: ModuleExpr -> RenameM Expr
    renameMod' mod = case mod of
      [] -> pure []
      (expr : mod') -> case expr of
        d@(MDecl v _) -> do
          d'@(MDecl v' _) <- rename d
          mod'' <- renameMod' (sub v (EVar v') mod')
          pure (d' : mod'')
        MExec {} -> do
          e' <- rename expr
          mod'' <- renameMod' mod'
          pure (e' : mod'')

reduceMod :: TypeEnv -> [ModuleExpr] -> Either ExprEvalError (ModuleEnv, Module)
reduceMod tyEnv = unwrap . mapAccumM accumModM Map.empty
  where
    unwrap m = runIdentity $ runExceptT (runReaderT m tyEnv)
    accumModM env expr = do
      expr' <- reduce (inEnv env expr)
      pure (merge expr' env, expr')

typeMod :: Module -> Either (InferenceError Type Expr) TypeEnv
typeMod = runExcept . typeMod' mkCEnv
  where
    typeMod' :: TypeEnv -> Module -> Except (InferenceError Type Expr) TypeEnv
    typeMod' env mod = case mod of
      [] -> pure env
      (expr : mod') -> do
        (_, env', t) <- liftEither (constraintsIn env (toExpr expr))
        env'' <- typeMod' env' mod'
        pure $
          Map.union env'' $ case expr of
            MDecl v _ -> Map.singleton v t
            MExec _ -> Map.empty

eval :: Module -> Either ModuleEvalError Module
eval mod = case typeMod mod of
  Left err -> Left $ MTypeError err
  Right tyEnv -> case reduceMod tyEnv mod of
    Left err -> Left $ MExprEvalError err
    Right (_, mod') -> Right mod'

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Mean.Evaluation.Module where

import Control.Category ((>>>))
import Control.Monad (MonadPlus (..), foldM, forM, (<=<))
import Control.Monad.Except (Except, liftEither, runExcept, runExceptT, throwError, withExceptT)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (ReaderT (runReaderT), ask)
import Control.Monad.State (evalStateT)
import Data.Foldable (Foldable (foldl'))
import Data.Function ((&))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace (trace, traceM)
import Mean.Context
import Mean.Context (Contextual)
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

toExpr = \case MDecl _ e -> e; MExec e -> e

instance Reducible ModuleExpr ModuleExpr ExprEvalError TypeEnv where
  reduce = \case
    MDecl v e -> MDecl v <$> reduce e
    MExec e -> MExec <$> reduce e

instance Contextual ModuleExpr where
  fv = \case
    MDecl _ e -> fv e
    MExec e -> fv e
  bv = \case
    MDecl v e -> Set.singleton v
    _ -> Set.empty

instance Substitutable Expr ModuleExpr where
  sub v expr mExpr =
    let sub' = sub v expr
     in case mExpr of
          MDecl v e -> MDecl v (sub' e)
          MExec e -> MExec (sub' e)

instance Renamable ModuleExpr where
  rename' vs mExpr = case mExpr of
    MDecl v e -> MDecl v <$> rename' vs e
    MExec e -> MExec <$> rename' vs e

merge modExpr modEnv = Map.union modEnv $ case modExpr of
  MDecl v e -> Map.singleton v e
  _ -> Map.empty

renameMod :: Module -> Module
renameMod mod = run $ (thdPass <=< sndPass <=< fstPass) mod
  where
    run m = runIdentity $ evalStateT m 0

    bvPre = bv mod
    bndEnv mod = Map.fromList $ [(reset v, EVar v) | v <- Set.toList $ bv mod]

    -- rename topmost let vars
    fstPass :: Module -> RenameM Module
    fstPass mod = forM mod $ \case
      MDecl v e -> do
        v' <- next v
        pure $ MDecl v' e
      mExpr -> pure mExpr

    -- update topmost let vars bound in every expr
    sndPass :: Module -> RenameM Module
    sndPass mod = pure $ inEnv (bndEnv mod) <$> mod

    -- now rename each expr ignoring the bound let vars
    thdPass :: Module -> RenameM Module
    thdPass mod =
      let rename e = rename' (fv e Set.\\ bv mod) e
       in mapM rename mod

typeMod :: Module -> Either (InferenceError Type Expr) TypeEnv
typeMod = runExcept . foldM typeMod' mkCEnv
  where
    typeMod' :: TypeEnv -> ModuleExpr -> Except (InferenceError Type Expr) TypeEnv
    typeMod' env expr = do
      traceM ("TYPING: " ++ show expr ++ " in " ++ show env)
      (_, env', t) <- liftEither (constraintsIn env (toExpr expr))
      traceM "OK"
      pure $
        Map.union env' $ case expr of
          MDecl v _ -> Map.singleton v t
          MExec _ -> Map.empty

reduceMod :: Module -> TypeEnv -> Either ExprEvalError (ModuleEnv, Module)
reduceMod mod tyEnv = unwrapT (mapAccumM accumModM Map.empty mod)
  where
    unwrapT m = runIdentity $ runExceptT (runReaderT m tyEnv)
    accumModM env expr = do
      expr' <- reduce (inEnv env expr)
      pure (merge expr' env, expr')

eval :: Module -> Either ModuleEvalError Module
eval mod =
  let mod' = renameMod mod
   in trace ("\nMOD:\n" ++ show mod') $ case typeMod mod' of
        Left err -> Left $ MTypeError err
        Right tyEnv -> trace ("\nTyEnv:\n" ++ show tyEnv) $ case reduceMod mod' tyEnv of
          Left err -> Left $ MExprEvalError err
          Right (_, mod'') -> Right mod''

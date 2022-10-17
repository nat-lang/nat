{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Mean.Evaluation.Module where

import Control.Monad ((<=<))
import Control.Monad.Except (runExceptT, withExceptT)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (ReaderT (runReaderT))
import qualified Data.Map as Map
import Debug.Trace (trace)
import Mean.Context
import Mean.Evaluation.Surface
import Mean.Evaluation.Type
import Mean.Inference
import Mean.Reduction
import Mean.Syntax.Module
import Mean.Syntax.Surface
import Mean.Syntax.Type
import Mean.Unification

data ModuleEvalError
  = MExprEvalError ExprEvalError
  | MTypeError TypeEnv Expr (InferenceError Type Expr)
  deriving (Eq, Show)

type ModuleExprReduction = Reduction ModuleExpr ExprEvalError (TypeEnv, ModuleEnv)

type ModuleEnv = Map.Map Var Expr

merge mod env = Map.union env $ case mod of
  MDecl v e -> Map.singleton v e
  _ -> Map.empty

instance Reducible Module Module ExprEvalError (TypeEnv, ModuleEnv) where
  reduce = mapM reduceM
    where
      reduce' expr tyEnv modEnv =
        let expr' = runReduce' tyEnv (inEnv modEnv expr)
         in case expr' of
              Left err -> throwError $ MExprEvalError err
              Right expr'' -> expr''

      reduceM :: ModuleExpr -> ModuleExprReduction
      reduceM expr = do
        (tyEnv, modEnv) <- ask
        expr' <- case expr of
          MDecl v e -> MDecl v <$> reduce' tyEnv modEnv e
          MExec e -> MExec <$> reduce' tyEnv modEnv e
        put (tyEnv, merge expr' modEnv)
        pure expr'

tyMod = runInference . mapM principal

typeModule :: TypeEnv -> Module -> Either ModuleEvalError TypeEnv
typeModule env mod = case mod of
  [] -> Right env
  (mExpr : mExprs) -> case mExpr of
    MExec {} -> Right env
    MDecl v e -> case runInferenceIn env e of
      Left err -> Left $ MTypeError env e err
      Right ty -> typeModule (extend env (v, ty)) mExprs

eval :: Module -> Either ModuleEvalError Module
eval m = case typeModule mkCEnv m of
  Left err -> Left err
  Right tyEnv -> case runReduce' (tyEnv, Map.empty) of
    Left err -> Left $ MExprEvalError err
    Right m -> Right m

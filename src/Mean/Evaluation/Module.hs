{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Mean.Evaluation.Module where

import Control.Monad ((<=<))
import Control.Monad.Except (runExceptT, withExceptT)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (ReaderT (runReaderT))
import qualified Data.Map as Map
import Debug.Trace (trace)
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

instance Reducible Module Module ExprEvalError TypeEnv where
  reduce mod = reduce' mod []
    where
      reduceIn :: Module -> Expr -> Reduction Expr ExprEvalError TypeEnv
      reduceIn mod = reduce . substitute (Map.fromList [(v, e) | MDecl v e <- mod])

      reduce' :: Module -> Module -> Reduction Module ExprEvalError TypeEnv
      reduce' mod mod' = case mod of
        [] -> pure mod'
        (expr : exprs) ->
          let next m = reduce' exprs (mod' ++ [m])
           in case expr of
                MDecl v e -> next <=< (pure . MDecl v) <=< reduceIn mod' $ e
                MExec e -> next <=< (pure . MExec) <=< reduceIn mod' $ e

typeModule :: TypeEnv -> Module -> Either ModuleEvalError TypeEnv
typeModule env mod = case mod of
  [] -> Right env
  (mExpr : mExprs) -> case mExpr of
    MExec {} -> Right env
    MDecl v e -> case inferIn env e of
      Left err -> Left $ MTypeError env e err
      Right ty -> typeModule (extend env (v, ty)) mExprs

eval :: Module -> Either ModuleEvalError Module
eval m = case typeModule mkCEnv m of
  Left err -> trace "foo" $ Left err
  Right env -> case runIdentity $ runExceptT $ runReaderT (reduce m) env of
    Left err -> trace "bar" $ Left $ MExprEvalError err
    Right m -> Right m

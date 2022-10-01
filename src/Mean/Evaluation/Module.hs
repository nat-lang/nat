{-# LANGUAGE FlexibleInstances #-}

module Mean.Evaluation.Module where

import Control.Monad ((<=<))
import Control.Monad.Except (runExceptT, withExceptT)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (ReaderT (runReaderT))
import qualified Data.Map as Map
import Debug.Trace (trace)
import Mean.Evaluation.Surface
import Mean.Evaluation.Type
import Mean.Syntax.Module
import Mean.Syntax.Surface
import Mean.Unification

data ModuleEvalError
  = MExprEvalError ExprEvalError
  | MTypeError TypeError
  deriving (Eq, Show)

instance Reducible Module where
  reduce mod = reduce' mod []
    where
      reduceIn :: Module -> Expr -> Evaluation Expr
      reduceIn mod = reduce . substitute (Map.fromList [(v, e) | MDecl v e <- mod])

      reduce' :: Module -> Module -> Evaluation Module
      reduce' mod mod' = case mod of
        [] -> pure mod'
        (expr : exprs) ->
          let next m = reduce' exprs (mod' ++ [m])
           in case expr of
                MDecl v e -> next <=< (pure . MDecl v) <=< reduceIn mod' $ e
                MExec e -> next <=< (pure . MExec) <=< reduceIn mod' $ e

typeModule :: TyEnv -> Module -> Either TypeError TyEnv
typeModule env mod = case mod of
  [] -> Right env
  (mExpr : mExprs) -> case mExpr of
    MExec {} -> Right env
    MDecl v e -> case infer' env e of
      Left err -> Left err
      Right ty -> typeModule (extend env (v, ty)) mExprs

eval :: Module -> Either ModuleEvalError Module
eval m = case typeModule mkTyEnv m of
  Left err -> Left $ MTypeError err
  Right env -> case runIdentity $ runExceptT $ runReaderT (reduce m) env of
    Left err -> Left $ MExprEvalError err
    Right m -> Right m

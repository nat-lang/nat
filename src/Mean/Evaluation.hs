{-# LANGUAGE FlexibleContexts #-}

module Mean.Evaluation where

import Control.Monad.Except
import Control.Monad.Identity
import qualified Data.Map as Map
import Data.Functor ((<&>))

import qualified Mean.Syntax as S

import Debug.Trace (traceM)

data EvalError = UnboundVariable S.Name | NotAFn S.Expr S.Expr

type Evaluation = ExceptT EvalError Identity
type EvalCtx = Map.Map S.Name S.Expr

eval :: EvalCtx -> S.Expr -> Evaluation S.Expr
eval ctx expr = let
  bind :: S.Binder -> S.Expr -> EvalCtx
  bind (S.Binder n _) e = Map.insert n e ctx

  in case expr of
    S.ELit{} -> pure expr
    S.Lam{} -> pure expr

    S.Var n -> case Map.lookup n ctx of
      Nothing -> throwError $ UnboundVariable n
      Just v -> pure v

    --  Call by name beta reduction.
    S.App e0 e1 -> case e0 of
      S.Lam b body -> eval (bind b e1) e0
      S.App{} -> do
        e0' <- eval ctx e0
        eval ctx (S.App e0' e1)
      _ -> throwError $ NotAFn e0 e1

emptyCtx = Map.empty

runEvalIn :: EvalCtx -> S.Expr -> Either EvalError S.Expr
runEvalIn ctx e = runIdentity $ runExceptT $ eval ctx e

runEval :: S.Expr -> Either EvalError S.Expr
runEval = runEvalIn emptyCtx

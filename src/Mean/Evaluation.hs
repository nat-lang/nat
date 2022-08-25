{-# LANGUAGE FlexibleContexts #-}

module Mean.Evaluation where

{-
import Control.Monad.Except
import Control.Monad.Identity
import qualified Data.Map as Map
import Data.Functor ((<&>))

import qualified Mean.Syntax as S

import Debug.Trace (traceM)

data Value
  = VInt Integer
  | VBool Bool

instance Show Value where
  show (VInt x) = show x
  show (VBool x) = show x

data EvalError
  = UnboundVariable S.Expr
  | NotAFn S.Expr S.Expr
  | NotAFormula Value

instance Show EvalError where
  show (UnboundVariable e) = "Unbound variable: " ++ show e
  show (NotAFn e0 e1) = "Tried to apply non fn: " ++ show e0 ++ " to " ++ show e1
  show (NotAFormula v) = "Expecting a formula. Got: " ++ show v

type Evaluation = ExceptT EvalError Identity
type EvalCtx = Map.Map S.Name Value

eval :: EvalCtx -> S.Expr -> Evaluation Value
eval ctx expr = let

  guardBool e = eval ctx e >>= \(VBool b) -> pure b
  guardInt e = eval ctx e >>= \(VInt i) -> pure $ fromIntegral i

  bind :: S.Binder -> EvalCtx
  bind (S.Binder n _) = Map.insert n (VFormula $ S.Var n) ctx

  arithmeticFormula op e0 e1 = do
    i0 <- guardInt e0
    i1 <- guardInt e1
    pure $ (VFormula . S.ELit . S.LInt) (op i0 i1)

  in case expr of
    l@(S.ELit (S.LInt _)) -> pure $ VFormula l
    l@(S.ELit (S.LBool _)) -> pure $ VFormula l

    -- f we've hit a variable then it has passed through
    -- type checking and beta reduction. If it's in context
    -- then one of the following is true: it has been
    -- (a) bound by a quantifier
    -- (b) bound during simplification of a lambda body
    -- (c) supplied by the caller (i.e. composition)
    S.Var n -> do
      -- traceM ("looking for: " ++ n)
      case Map.lookup n ctx of
        Nothing -> throwError $ UnboundVariable expr
        Just v -> pure v

    S.Lam b e -> do
      e' <- simplify b e
      pure $ VFunc (S.Lam b e')

    -- | Cbn beta reduction. We may "fallthru" in case
    --    (a) a constant is applied to an expr, or
    --    (b) a variable is applied to an expr.
    --   (a) is an instance of a predicate, while (b) happens during simplification.
    a@(S.App e0 e1) -> case e0 of
      a0@(S.App _ _) -> do
        lhs <- eval ctx a0
        case lhs of
          VFunc (S.Lam (S.Binder n _) body) -> betaReduce n body
          _ -> fallthru
      S.Lam (S.Binder n _) body -> betaReduce n body
      _ -> fallthru
      where
        fallthru = pure $ VFormula $ S.resolvePredicates a
        betaReduce arg body = eval ctx $ S.resolvePredicates $ S.substitute e1 arg body

extendCtx :: S.Name -> Value -> EvalCtx -> EvalCtx
extendCtx = Map.insert

emptyCtx = Map.empty

runEvalIn :: EvalCtx -> S.Expr -> Either EvalError Value
runEvalIn ctx e = runIdentity $ runExceptT $ eval ctx e

runEval :: S.Expr -> Either EvalError Value
runEval = runEvalIn emptyCtx

-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}

module Interpreter.Evaluation where

import Control.Monad.Except
import Control.Monad.Identity
import qualified Data.Map as Map

import Compiler.Core.Pretty
import qualified Compiler.Core.Syntax as Syn

import Debug.Trace (traceM)

data Value
  = VInt Integer
  | VBool Bool
  | VFormula Syn.Expr
  | VFunc Syn.Expr
  -- supports AST operations on frontend via composition
  | VBinder Syn.Binder

instance Show Value where
  show (VInt x) = show x
  show (VFormula exp) = show exp
  show (VBool x) = show x
  show (VFunc exp) = show exp
  show (VBinder b) = "λ" ++ show b

data EvalError
  = UnboundVariable Syn.Expr
  | NotAFn Syn.Expr Syn.Expr
  | NotAFormula Value

instance Show EvalError where
  show (UnboundVariable e) = "Unbound variable: " ++ show e
  show (NotAFn e0 e1) = "Tried to apply non fn: " ++ show e0 ++ " to " ++ show e1
  show (NotAFormula v) = "Expecting a formula. Got: " ++ show v

type Evaluation = ExceptT EvalError Identity
type EvalCtx = Map.Map Syn.Name Value

eval :: EvalCtx -> Syn.Expr -> Evaluation Value
eval ctx expr = let

  guardBool e = eval ctx e >>= \v -> case v of
    VBool b -> pure b
  guardInt e = eval ctx e >>= \v -> case v of
    VInt i -> pure $ fromIntegral i
  guardForm ctx' e = eval ctx' e >>= \v -> case v of
    VFormula e' -> pure e'
    v -> throwError $ NotAFormula v

  subformula :: Syn.UnOp -> Syn.Expr -> Evaluation Value
  subformula f e = guardForm ctx e >>= pure . VFormula . (Syn.EUnOp f)

  subformulae :: ([Syn.Expr] -> Syn.Expr) -> [Syn.Expr] -> Evaluation Value
  subformulae f es = mapM (guardForm ctx) es >>= pure . VFormula . f 

  subformulae2 :: (Syn.Expr -> Syn.Expr -> Syn.Expr) -> Syn.Expr -> Syn.Expr -> Evaluation Value
  subformulae2 op e0 e1 = do
    f0 <- (guardForm ctx e0)
    f1 <- (guardForm ctx e1)
    pure $ VFormula $ op f0 f1
  
  bind :: Syn.Binder -> EvalCtx
  bind (Syn.Binder n _) = Map.insert n (VFormula $ Syn.Var n) ctx

  simplify :: Syn.Binder -> Syn.Expr -> Evaluation Syn.Expr
  simplify b e = do
    traceM ("simplifying.." ++ show e)
    v <- eval (bind b) e
    traceM ("finished simplifying..")
    case v of
      VFormula e' -> do
        traceM ("\t to vform: " ++ show e')
        pure $ e'
      VFunc e' -> do
        traceM ("\t to vfun: " ++ show e')
        pure $ e'
      _ -> pure $ expr

  arithmeticFormula op e0 e1 = do
    i0 <- guardInt e0
    i1 <- guardInt e1
    pure $ (VFormula . Syn.ELit . Syn.LInt) (op i0 i1)

  in case expr of
    l@(Syn.ELit (Syn.LInt _)) -> pure $ VFormula l
    l@(Syn.ELit (Syn.LBool _)) -> pure $ VFormula l

    -- f we've hit a variable then it has passed through
    -- type checking and beta reduction. If it's in context
    -- then one of the following is true: it has been
    -- (a) bound by a quantifier
    -- (b) bound during simplification of a lambda body
    -- (c) supplied by the caller (i.e. composition)
    Syn.Var n -> do
      traceM ("looking for: " ++ n)
      case Map.lookup n ctx of
        Nothing -> throwError $ UnboundVariable expr
        Just v -> pure v

    c@(Syn.Const _ _) -> pure $ VFormula c

    {-
    Syn.EUnOp op -> case op of
      Syn.Neg e -> negate e where
        negate = pure . VBool . not . guardBool
    -}

    Syn.EUnOp q e -> subformula q e

    Syn.EBinOp op e0 e1 -> case op of
      Syn.Add -> arithmeticFormula (+) e0 e1
      Syn.Mul -> arithmeticFormula (*) e0 e1
      Syn.Sub -> arithmeticFormula (-) e0 e1
      Syn.Div -> arithmeticFormula (div) e0 e1
      _ -> subformulae2 (Syn.EBinOp op) e0 e1

    Syn.EComparison c e0 e1 -> subformulae2 (Syn.EComparison c) e0 e1

    Syn.Pred n t es -> subformulae (Syn.Pred n t) es

    Syn.EBinder b -> pure $ VBinder b

    Syn.EQuant q b e -> do
      e' <- guardForm (bind b) e
      pure $ VFormula $ Syn.EQuant q b e'

    Syn.Lam b e -> do
      e' <- simplify b e
      pure $ VFunc (Syn.Lam b e')

    -- | Cbn beta reduction. We may "fallthru" in case
    --    (a) a constant is applied to an expr, or
    --    (b) a variable is applied to an expr.
    --   (a) is an instance of a predicate, while (b) happens during simplification.
    a@(Syn.App e0 e1) -> case e0 of
      a0@(Syn.App _ _) -> do
        lhs <- eval ctx a0
        case lhs of
          VFunc (Syn.Lam (Syn.Binder n _) body) -> betaReduce n body
          _ -> fallthru
      Syn.Lam (Syn.Binder n _) body -> betaReduce n body
      _ -> fallthru
      where
        fallthru = pure $ VFormula $ Syn.resolvePredicates a
        betaReduce arg body = eval ctx $ Syn.resolvePredicates $ Syn.substitute e1 arg body

extendCtx :: Syn.Name -> Value -> EvalCtx -> EvalCtx
extendCtx = Map.insert

emptyCtx = Map.empty

runEvalIn :: EvalCtx -> Syn.Expr -> Either EvalError Value
runEvalIn ctx e = runIdentity $ runExceptT $ eval ctx e

runEval :: Syn.Expr -> Either EvalError Value
runEval = runEvalIn emptyCtx


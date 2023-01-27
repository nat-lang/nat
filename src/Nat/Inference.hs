{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Nat.Inference where

import Control.Monad.Except (Except, MonadError (throwError), liftEither, runExcept)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State (MonadState, StateT, evalStateT, gets, modify, runStateT, state)
import Data.Either (fromLeft, fromRight)
import Data.List (intercalate)
import qualified Data.Map as Map
import Debug.Trace (trace, traceM)
import qualified Nat.Context as C
import Nat.Unification
import Nat.Viz
import Text.PrettyPrint (nest, (<+>))

data InferenceError a b
  = IUnconstrainable b
  | IUnboundVariable C.Var (ConstraintEnv a)
  | IUnificationError b (UnificationError a)
  | IInexhaustiveCase a
  | IContextualInferenceError (Map.Map b a) (InferenceError a b) [Constraint a]
  deriving (Eq)

instance (Pretty a, Pretty b, Show a, Show b) => Pretty (InferenceError a b) where
  ppr _ e = case e of
    IContextualInferenceError env err cs ->
      text (show err)
        <+> text "where\nenv:\n"
        <+> prettyPairs "->" (Map.toList env)
        <+> "\nconstraints:\n"
        <+> pp cs
    IUnconstrainable b -> text "Unconstrainable:" <+> text (show b)
    IUnboundVariable v env -> text "Unbound variable" <+> text (show v ++ " in " ++ show env)
    IUnificationError b err -> text "When inferring the type of" <+> text (show b ++ ":\n\t" ++ show err)
    IInexhaustiveCase b -> text "Inexhaustive case:" <+> text (show b)

instance (Pretty a, Pretty b, Show a, Show b) => Show (InferenceError a b) where
  show = show . ppr 0

type Constraint a = (a, a)

type ConstraintEnv a = Map.Map C.Var a

data InferenceState a b = IState {names :: Int, assignments :: Map.Map b a}

mkIState = IState {names = 0, assignments = Map.empty}

-- | Inference stack
type InferT a b r =
  ( ReaderT
      (ConstraintEnv a) -- initial environment parameterized over a
      ( StateT
          (InferenceState a b) -- name supply, incremental assignments
          ( Except
              (InferenceError a b) -- errors
          )
      )
      r -- result
  )

runInferT :: InferenceState a b -> ConstraintEnv a -> Constrain a b -> Either (InferenceError a b) ((a, [Constraint a]), InferenceState a b)
runInferT s env m = runExcept $ runStateT (runReaderT m env) s

evalInferT :: InferenceState a b -> ConstraintEnv a -> Constrain a b -> Either (InferenceError a b) (a, [Constraint a])
evalInferT s env m = fmap fst (runInferT s env m)

type Constrain a b = InferT a b (a, [Constraint a])

unwrapSignature = \case
  Left err -> Left err
  Right (_, env, _) -> Right env

-- | Infer an "a" from a "b"
class (Unifiable a, Show a, Show b, Ord b, Pretty b) => Inferrable a b where
  constrain' :: b -> Constrain a b

  constrain :: b -> Constrain a b
  constrain b = do
    (t, cs) <- constrain' b
    modify (\s -> s {assignments = Map.insert b t (assignments s)})
    return (t, cs)

  liftUnify :: [Constraint a] -> b -> Either (InferenceError a b) (ConstraintEnv a)
  liftUnify cs b = case runUnify cs of
    Left e -> Left $ IUnificationError b e
    Right s -> Right s

  principal' :: b -> Constrain a b
  principal' b = do
    (a, cs) <- constrain b
    s <- gets assignments
    traceM ("Env:\n" ++ (show $ prettyPairs " = " (Map.toList s)))
    case liftUnify cs b of
      Left e -> throwError (IContextualInferenceError s e cs)
      Right signature -> return (C.inEnv signature a, cs)

  principal :: b -> Constrain a b
  principal = principal'

  signify :: b -> InferT a b (a, ConstraintEnv a)
  signify b = do
    (a, cs) <- principal b
    s <- liftEither (liftUnify cs b)
    return (C.inEnv s a, s)

  runInference' ::
    InferenceState a b ->
    b ->
    ConstraintEnv a ->
    Either (InferenceError a b) (a, [Constraint a])
  runInference' s a e = evalInferT s e (principal a)

  runInference ::
    b ->
    ConstraintEnv a ->
    Either (InferenceError a b) (a, [Constraint a])
  runInference = runInference' (IState {names = 0, assignments = Map.empty})

  inferIn :: ConstraintEnv a -> b -> Either (InferenceError a b) a
  inferIn env b = case runInference b env of
    Left e -> Left e
    Right (a, _) -> Right a

  infer :: b -> Either (InferenceError a b) a
  infer b = inferIn Map.empty b

  -- | Return the constraints used to make an inference in a given state and environment
  constraintsIn ::
    b ->
    ConstraintEnv a ->
    Either (InferenceError a b) ([Constraint a], ConstraintEnv a, a)
  constraintsIn expr env = do
    (t, cs) <- liftEither (runInference expr env)
    s <- liftUnify cs expr
    pure (cs, s, t)

  -- | Return the constraints used to make an inference
  constraints ::
    b ->
    Either
      (InferenceError a b)
      ([Constraint a], ConstraintEnv a, a)
  constraints e = constraintsIn e Map.empty

  runSignifyIn :: b -> ConstraintEnv a -> Either (InferenceError a b) (ConstraintEnv a)
  runSignifyIn env = unwrapSignature . constraintsIn env

  -- | Return the type signature an inference produces
  runSignify :: b -> Either (InferenceError a b) (ConstraintEnv a)
  runSignify = unwrapSignature . constraints

  fresh' :: InferT a b Int
  fresh' = do
    i <- gets names
    modify (\s -> s {names = i + 1})
    return (i + 1)

  fresh :: InferT a b a
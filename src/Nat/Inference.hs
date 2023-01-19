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
  | IContextualInferenceError (Map.Map b a) (InferenceError a b)
  deriving (Eq)

instance (Show a, Show b) => Pretty (InferenceError a b) where
  ppr _ e = case e of
    IContextualInferenceError env err -> text (show err) <+> text "where\n" <+> pp env
      where
        pp env = text $ intercalate "\n" (fmap (\(k, v) -> "\t" ++ show k ++ " = " ++ show v) (Map.toList env))
    IUnconstrainable b -> text "Unconstrainable:" <+> text (show b)
    IUnboundVariable v env -> text "Unbound variable" <+> text (show v ++ " in " ++ show env)
    IUnificationError b err -> text "Can't unify" <+> text (show b ++ ": " ++ show err)
    IInexhaustiveCase b -> text "Inexhaustive case:" <+> text (show b)

instance (Show a, Show b) => Show (InferenceError a b) where
  show = show . ppr 0

type Constraint a = (a, a)

type ConstraintEnv a = Map.Map C.Var a

data InferenceState a b = IState {names :: Int, types :: Map.Map b a}

mkIState = IState {names = 0, types = Map.empty}

-- | Inference stack
type InferT a b r =
  ( ReaderT
      (ConstraintEnv a) -- initial environment parameterized over a
      ( StateT
          (InferenceState a b) -- name supply, incremental principal types
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

-- | Infer a from b
class (Unifiable a, Show a, Show b, Ord b) => Inferrable a b where
  constrain :: b -> Constrain a b

  unify' :: [Constraint a] -> b -> Either (InferenceError a b) (ConstraintEnv a)
  unify' cs b = case runUnify cs of
    Left e -> Left $ IUnificationError b e
    Right s -> Right s

  -- | Calculate incremental principal types
  principal' :: b -> Constrain a b
  principal' b = do
    (a, cs) <- constrain b
    case unify' cs b of
      Left e -> do
        s <- gets types
        throwError (IContextualInferenceError s e)
      Right u -> do
        let mgu = C.inEnv u a
        modify (\s -> s {types = Map.insert b mgu (types s)})
        return (mgu, cs)

  principal :: b -> Constrain a b
  principal = principal'

  signify :: b -> InferT a b (a, ConstraintEnv a)
  signify b = do
    (a, cs) <- principal b
    s <- liftEither (unify' cs b)
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
  runInference = runInference' (IState {names = 0, types = Map.empty})

  inferIn :: ConstraintEnv a -> b -> Either (InferenceError a b) a
  inferIn env b = case runInference b env of
    Left e -> Left e
    Right (a, _) -> Right a

  infer :: b -> Either (InferenceError a b) a
  infer b = trace ("Inferring the type of " ++ show b) $ inferIn Map.empty b

  -- | Return the constraints used to make an inference in a given state and environment
  constraintsIn ::
    b ->
    ConstraintEnv a ->
    Either (InferenceError a b) ([Constraint a], ConstraintEnv a, a)
  constraintsIn expr env = do
    (t, cs) <- liftEither (runInference expr env)
    s <- unify' cs expr
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
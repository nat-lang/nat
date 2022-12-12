{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Nat.Inference where

import Control.Monad.Except (Except, MonadError (throwError), liftEither, runExcept)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State (MonadState, StateT, evalStateT, runStateT, state)
import Data.Either (fromLeft, fromRight)
import qualified Data.Map as Map
import Debug.Trace (traceM)
import qualified Nat.Context as C
import Nat.Unification

data InferenceError a b
  = IUnconstrainable b
  | IUnboundVariable C.Var (ConstraintEnv a)
  | IUnificationError b (UnificationError a)
  | IInexhaustiveCase a
  deriving (Eq, Show)

type Constraint a = (a, a)

type ConstraintEnv a = Map.Map C.Var a

mkCEnv :: Map.Map k a
mkCEnv = Map.empty

-- | Inference stack
type ConstrainT a b r =
  ( ReaderT
      (ConstraintEnv a) -- environment parameterized over a
      ( C.FreshT -- name supply
          ( Except
              (InferenceError a b) -- errors
          )
      )
      r -- result
  )

runConstrainT :: Int -> ConstraintEnv a -> Constrain a b -> Either (InferenceError a b) ((a, [Constraint a]), Int)
runConstrainT s env m = runExcept $ runStateT (runReaderT m env) s

evalConstrainT :: Int -> ConstraintEnv a -> Constrain a b -> Either (InferenceError a b) (a, [Constraint a])
evalConstrainT s env m = fmap fst (runConstrainT s env m)

type Constrain a b = ConstrainT a b (a, [Constraint a])

unwrapSignature = \case
  Left err -> Left err
  Right (_, env, _) -> Right env

-- | Infer a from b
class (Unifiable a, Show a, Show b) => Inferrable a b where
  constrain :: b -> Constrain a b

  unify' :: [Constraint a] -> b -> Either (InferenceError a b) (ConstraintEnv a)
  unify' cs b = case runUnify cs of
    Left e -> Left $ IUnificationError b e
    Right s -> Right s

  -- | Calculate incremental principal types
  principal' :: b -> Constrain a b
  principal' b = do
    (a, cs) <- constrain b
    s <- liftEither (unify' cs b)
    pure (C.inEnv s a, cs)

  principal :: b -> Constrain a b
  principal = principal'

  signify :: b -> ConstrainT a b (a, ConstraintEnv a)
  signify b = do
    (a, cs) <- principal b
    s <- liftEither (unify' cs b)
    pure (C.inEnv s a, s)

  runInference' ::
    Int ->
    b ->
    ConstraintEnv a ->
    Either (InferenceError a b) (a, [Constraint a])
  runInference' s a e = evalConstrainT s e (principal a)

  runInference ::
    b ->
    ConstraintEnv a ->
    Either (InferenceError a b) (a, [Constraint a])
  runInference = runInference' 0

  inferIn :: ConstraintEnv a -> b -> Either (InferenceError a b) a
  inferIn env b = case runInference b env of
    Left e -> Left e
    Right (a, _) -> Right a

  infer :: b -> Either (InferenceError a b) a
  infer = inferIn mkCEnv

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
  constraints e = constraintsIn e mkCEnv

  runSignifyIn :: b -> ConstraintEnv a -> Either (InferenceError a b) (ConstraintEnv a)
  runSignifyIn env = unwrapSignature . constraintsIn env

  -- | Return the type signature an inference produces
  runSignify :: b -> Either (InferenceError a b) (ConstraintEnv a)
  runSignify = unwrapSignature . constraints

  fresh' :: ConstrainT a b Int
  fresh' = state C.fresh'

  fresh :: ConstrainT a b a
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Mean.Inference where

import Control.Monad.Except (Except, MonadError (throwError), liftEither, runExcept)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State (MonadState (get), StateT, evalStateT)
import Data.Either (fromLeft)
import qualified Data.Map as Map
import Debug.Trace (traceM)
import Mean.Context
import Mean.Unification

data InferenceError a b
  = IUnconstrainable b
  | IUnboundVariable Var (ConstraintEnv a)
  | IUnificationError b (UnificationError a)
  | IInexhaustiveCase [a]
  deriving (Eq, Show)

type Constraint a = (a, a)

type ConstraintEnv a = Map.Map Var a

mkCEnv :: Map.Map k a
mkCEnv = Map.empty

-- | Constraint generation monad stack
type ConstrainT a b s r =
  ( ReaderT
      (ConstraintEnv a) -- environment parameterized over a
      ( StateT
          s -- arbitrary state
          ( Except
              (InferenceError a b) -- errors
          )
      )
      r -- result
  )

type Constrain a b s = ConstrainT a b s (a, [Constraint a])

unwrapSignature = \case
  Left err -> Left err
  Right (_, env, _) -> Right env

-- | Infer a from b in state s.
class Unifiable a => Inferrable a b s | b -> s where
  constrain :: b -> Constrain a b s

  unify' :: [Constraint a] -> b -> Either (InferenceError a b) (ConstraintEnv a)
  unify' cs b = case runUnify cs of
    Left e -> Left $ IUnificationError b e
    Right s -> Right s

  -- | Calculate incremental principal types
  principal :: b -> Constrain a b s
  principal b = do
    (a, cs) <- constrain b
    s <- liftEither (unify' cs b)
    pure (inEnv s a, cs)

  signify :: b -> ConstrainT a b s (a, ConstraintEnv a)
  signify b = do
    (a, cs) <- principal b
    s <- liftEither (unify' cs b)
    pure (inEnv s a, s)

  -- | Run the inference monad
  runInference' ::
    s ->
    ConstraintEnv a ->
    Constrain a b s ->
    Either (InferenceError a b) (a, [Constraint a])
  runInference' s env m = runExcept $ evalStateT (runReaderT m env) s

  runInference ::
    ConstraintEnv a ->
    Constrain a b s ->
    Either (InferenceError a b) (a, [Constraint a])

  inferIn :: ConstraintEnv a -> b -> Either (InferenceError a b) a
  inferIn env b = case runInference env (principal b) of
    Left e -> Left e
    Right (a, _) -> Right a

  infer :: b -> Either (InferenceError a b) a
  infer = inferIn mkCEnv

  -- | Return the constraints used to make an inference in a given state and environment
  constraintsIn ::
    ConstraintEnv a ->
    b ->
    Either (InferenceError a b) ([Constraint a], ConstraintEnv a, a)
  constraintsIn env expr = do
    (t, cs) <- liftEither (runInference env (principal expr))
    s <- unify' cs expr
    pure (cs, s, t)

  -- | Return the constraints used to make an inference
  constraints ::
    b ->
    Either
      (InferenceError a b)
      ([Constraint a], ConstraintEnv a, a)
  constraints = constraintsIn mkCEnv

  runSignifyIn :: ConstraintEnv a -> b -> Either (InferenceError a b) (ConstraintEnv a)
  runSignifyIn env = unwrapSignature . constraintsIn env

  -- | Return the type signature an inference produces
  runSignify :: b -> Either (InferenceError a b) (ConstraintEnv a)
  runSignify = unwrapSignature . constraints

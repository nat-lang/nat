{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Mean.Inference where

import Control.Monad.Except (Except, MonadError (throwError), runExcept)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State (MonadState (get), StateT, evalStateT)
import qualified Data.Map as Map
import Debug.Trace (traceM)
import Mean.Context
import Mean.Unification

data InferenceError a b
  = IUnconstrainable b
  | IUnboundVariable Var (ConstraintEnv a)
  | IUnificationError b (UnificationError a)
  | IInexhaustiveCase b
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

-- | Infer a from b in state s.
class Unifiable a => Inferrable a b s | b -> s where
  constrain :: b -> Constrain a b s

  -- | Calculate incremental principal types
  principal :: b -> Constrain a b s
  principal b = do
    (a, cs) <- constrain b
    case runUnify cs of
      Left e -> throwError $ IUnificationError b e
      Right s -> do
        pure (inEnv s a, cs)

  -- | Run the constraint generation monad
  runConstrain ::
    ConstraintEnv a ->
    s ->
    Constrain a b s ->
    Either (InferenceError a b) (a, [Constraint a])
  runConstrain env s m = runExcept $ evalStateT (runReaderT m env) s

  -- | Return the constraints used to make an inference in a given state and environment
  constraintsIn' ::
    ConstraintEnv a ->
    s ->
    b ->
    Either (InferenceError a b) ([Constraint a], ConstraintEnv a, a)
  constraintsIn' env s expr = case runConstrain env s (constrain expr) of
    Left err -> Left err
    Right (c, cs) -> case runUnify cs of
      Left err -> Left $ IUnificationError expr err
      Right sub -> Right (cs, sub, c)

  -- | Return the constraints used to make an inference in a given environment
  constraintsIn ::
    ConstraintEnv a ->
    b ->
    Either
      (InferenceError a b)
      ([Constraint a], ConstraintEnv a, a)

  -- | Return the constraints used to make an inference
  constraints ::
    b ->
    Either
      (InferenceError a b)
      ([Constraint a], ConstraintEnv a, a)

  -- | Return the type signature an inference produces
  signify :: b -> Either (InferenceError a b) (ConstraintEnv a)
  signify b = case constraints b of
    Left err -> Left err
    Right (_, env, _) -> Right env

  -- | Infer with given environment and state
  runInference' ::
    ConstraintEnv a ->
    s ->
    b ->
    Either (InferenceError a b) a
  runInference' env s expr = case runConstrain env s (principal expr) of
    Left err -> Left err
    Right (a, _) -> pure a

  -- | Infer with given environment
  runInferenceIn :: ConstraintEnv a -> b -> Either (InferenceError a b) a

  -- | Infer
  runInference :: b -> Either (InferenceError a b) a
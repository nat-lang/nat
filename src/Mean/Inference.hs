{-# LANGUAGE FunctionalDependencies #-}

module Mean.Inference where

import Control.Monad.Except (Except, MonadError (throwError), runExcept)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State (MonadState (get), StateT, evalStateT)
import qualified Data.Map as Map
import Debug.Trace (traceM)
import Mean.Unification
import Mean.Var

data InferenceError a b
  = IUnconstrainable b
  | IUnboundVariable Var
  | IUnificationError b (UnificationError a)
  deriving (Eq, Show)

type Constraint a = (a, a)

type ConstraintEnv a = Map.Map Var a

mkCEnv :: Map.Map k a
mkCEnv = Map.empty

-- | Constraint generation monad stack
type ConstrainT a b s r =
  ( ReaderT
      (ConstraintEnv a) -- Environment parameterized over b
      ( StateT
          s -- Arbitrary state
          ( Except
              (InferenceError a b) -- Errors
          )
      )
      r -- Result
  )

type Constrain a b s = ConstrainT a b s (a, [Constraint a])

-- | Infer a from b.
class (Unifiable a, Show s) => Inferrable a b s | a -> s where
  constrain' :: b -> Constrain a b s

  constrain :: b -> Constrain a b s
  constrain b = do
    (a, cs) <- constrain' b
    case runUnify cs of
      Left e -> throwError $ IUnificationError b e
      Right s -> pure (substitute s a, cs)

  -- | Run the constraint generation monad
  runConstrain ::
    ConstraintEnv a ->
    s ->
    Constrain a b s ->
    Either (InferenceError a b) (a, [Constraint a])
  runConstrain env s m = runExcept $ evalStateT (runReaderT m env) s

  -- | Return the constraints used to make an inference
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

  -- | Infer with given state and environment
  infer' ::
    ConstraintEnv a ->
    s ->
    b ->
    Either (InferenceError a b) a
  infer' env s expr = case runConstrain env s (constrain expr) of
    Left err -> Left err
    Right (a, _) -> pure a

  -- | Infer with given environment
  inferIn :: ConstraintEnv a -> b -> Either (InferenceError a b) a

  -- | Infer with an unspecified environment
  infer :: b -> Either (InferenceError a b) a
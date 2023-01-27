{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Nat.Inference where

import Control.Monad.Except (Except, MonadError (throwError), liftEither, runExcept)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State (MonadState (get), StateT, evalStateT, gets, modify, runStateT, state)
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
  | IUnboundVariable C.Var (Signature a)
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

data InferenceState a b = IState {names :: Int, assignments :: Map.Map b a, signature :: Signature a}

mkIState = IState {names = 0, assignments = Map.empty}

-- | Inference stack
type InferT a b r =
  ( ReaderT
      (Signature a) -- initial environment parameterized over a
      ( StateT
          (InferenceState a b) -- name supply, incremental assignments
          ( Except
              (InferenceError a b) -- errors
          )
      )
      r -- result
  )

runInferT :: InferenceState a b -> Signature a -> Constrain a b -> Either (InferenceError a b) ((a, [Constraint a]), InferenceState a b)
runInferT s env m = runExcept $ runStateT (runReaderT m env) s

evalInferT :: InferenceState a b -> Signature a -> Constrain a b -> Either (InferenceError a b) (a, [Constraint a])
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

  liftUnify :: [Constraint a] -> b -> Either (InferenceError a b) (Signature a)
  liftUnify cs b = case runUnify cs of
    Left e -> Left $ IUnificationError b e
    Right s -> Right s

  principal :: b -> Constrain a b
  principal = principal'

  principal' :: b -> Constrain a b
  principal' b = do
    (t, cs) <- constrain b
    case liftUnify cs b of
      Left e -> do
        assigns <- gets assignments
        throwError (IContextualInferenceError assigns e cs)
      Right sig -> do
        let principalType = C.inEnv sig t
        modify (\s -> s {signature = sig, assignments = Map.insert b principalType (assignments s)})
        return (principalType, cs)

  signify :: b -> InferT a b (a, Signature a, Map.Map b a)
  signify b = do
    (t, cs) <- principal b
    s <- get

    return (C.inEnv (signature s) t, signature s, assignments s)

  runInference' ::
    InferenceState a b ->
    b ->
    Signature a ->
    Either (InferenceError a b) (a, [Constraint a])
  runInference' s a e = evalInferT s e (principal a)

  runInference ::
    b ->
    Signature a ->
    Either (InferenceError a b) (a, [Constraint a])
  runInference = runInference' (IState {names = 0, assignments = Map.empty})

  inferIn :: Signature a -> b -> Either (InferenceError a b) a
  inferIn env b = case runInference b env of
    Left e -> Left e
    Right (a, _) -> Right a

  infer :: b -> Either (InferenceError a b) a
  infer b = inferIn Map.empty b

  -- | Return the constraints used to make an inference in a given state and environment
  constraintsIn ::
    b ->
    Signature a ->
    Either (InferenceError a b) ([Constraint a], Signature a, a)
  constraintsIn expr env = do
    (t, cs) <- liftEither (runInference expr env)
    s <- liftUnify cs expr
    pure (cs, s, t)

  -- | Return the constraints used to make an inference
  constraints ::
    b ->
    Either
      (InferenceError a b)
      ([Constraint a], Signature a, a)
  constraints e = constraintsIn e Map.empty

  runSignifyIn :: b -> Signature a -> Either (InferenceError a b) (Signature a)
  runSignifyIn env = unwrapSignature . constraintsIn env

  -- | Return the type signature an inference produces
  runSignify :: b -> Either (InferenceError a b) (Signature a)
  runSignify = unwrapSignature . constraints

  fresh' :: InferT a b Int
  fresh' = do
    i <- gets names
    modify (\s -> s {names = i + 1})
    return (i + 1)

  fresh :: InferT a b a
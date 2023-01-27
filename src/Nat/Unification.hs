{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Nat.Unification where

import Control.Monad (foldM, mapM, mapM_, (<=<), (>=>))
import Control.Monad.Except (Except, ExceptT, runExcept, runExceptT, throwError)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.State (MonadState (get, put), StateT (runStateT), execStateT, gets, modify)
import Data.Either (isRight)
import Data.List (reverse, sort)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace (traceM)
import Nat.Context
import Nat.Viz
import Text.PrettyPrint
  ( Doc,
    text,
    (<+>),
  )
import Prelude hiding ((<*>))

type Signature a = Env a

data UnificationError a
  = NotUnifiable !a !a
  | ContextualUnificationError (UnificationError a) (Signature a)
  deriving (Eq)

tshow :: Show a => a -> Doc
tshow = text . show

instance Pretty a => Pretty (UnificationError a) where
  ppr _ (NotUnifiable e e') = text "Failed to unify the constraint:" <+> pp e <+> text "=" <+> pp e'
  ppr p (ContextualUnificationError err sig) = ppr p err <+> text "where\nsignature:\n" <+> pp sig

instance Pretty a => Show (UnificationError a) where
  show = show . ppr 0

type UnifyT a r = StateT (Signature a) (Except (UnificationError a)) r

type UnifyM a = UnifyT a (Signature a)

execUnifyT :: Signature a -> UnifyT a r -> Either (UnificationError a) (Signature a)
execUnifyT s m = runExcept (execStateT m s)

class (Substitutable a a, Show a, Pretty a, Ord a) => Unifiable a where
  unify :: a -> a -> UnifyM a

  -- | Simple unification doesn't need access to more than one
  -- constraint at a time.
  unify' :: [Pair a] -> Pair a -> UnifyM a
  unify' _ (a, b) = unify a b

  -- | Give context to a unification error.
  throwContextualError :: UnificationError a -> UnifyT a b
  throwContextualError err = do
    signature <- get
    throwError (ContextualUnificationError err signature)

  updateSignature :: Pair a -> [Pair a] -> UnifyT a [Pair a]
  updateSignature c cs = do
    unifier <- unify' cs c
    signature <- gets (compose unifier)
    put signature
    let cs' = inEnv signature cs
    -- traceM ("signature:\n" ++ (show $ pp signature))
    -- traceM "\n"
    return cs'

  unifyMany :: [Pair a] -> UnifyT a ()
  unifyMany =
    expandInvariants >=> log >=> \case
      [] -> pure mempty
      (c : cs) -> updateSignature c cs >>= unifyMany
    where
      log cs = do
        -- traceM ("constraints:\n" ++ (show $ pp cs))
        return cs

  runUnifyIn :: Signature a -> [Pair a] -> Either (UnificationError a) (Signature a)
  runUnifyIn sig = execUnifyT sig . unifyMany

  runUnify :: [Pair a] -> Either (UnificationError a) (Signature a)
  runUnify = runUnifyIn mkUState

  unifiable :: a -> a -> Bool
  unifiable t0 t1 = isRight $ runUnify [(t0, t1)]

  (<=>) :: a -> a -> Bool
  (<=>) = unifiable

  (<!>) :: a -> a -> Bool
  (<!>) a0 a1 = not (a0 <=> a1)

  mkUState :: Signature a
  mkUState = Map.empty

  expandInvariants :: [Pair a] -> UnifyT a [Pair a]
  expandInvariants cs = foldM aggregate [] cs
    where
      aggregate cs = (return . (cs ++)) <=< invariants

  -- default, to be overriden
  invariants :: Pair a -> UnifyT a [Pair a]
  invariants c = return [c]
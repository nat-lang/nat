{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Mean.Context where

import Control.Monad (replicateM)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.State (MonadState (get, put), StateT, evalStateT, runStateT, state)
import Data.Foldable (Foldable (foldl'))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Mean.Viz
import Mean.Walk
import Text.PrettyPrint
  ( char,
    text,
    (<>),
  )
import Prelude hiding ((<>))

type Name = String

data Var = Var !Name !Name

type Env a = Map.Map Var a

type Pair a = (a, a)

instance Eq Var where
  (==) :: Var -> Var -> Bool
  (Var _ v) == (Var _ v') = v == v'

instance Ord Var where
  compare :: Var -> Var -> Ordering
  compare (Var _ v0) (Var _ v1) = compare v0 v1

instance Show Var where
  show :: Var -> String
  show (Var vPub vPri) = vPub ++ "(" ++ vPri ++ ")"

instance Pretty [Var] where
  ppr p (v : vs) = text (show v) <> char ',' <> text (show v)
  ppr p [] = text ""

mkVar :: Name -> Var
mkVar v = Var v v

mkEnv :: Var -> a -> Env a
mkEnv = Map.singleton

reset :: Var -> Var
reset (Var vPre _) = Var vPre vPre

class Substitutable a b where
  sub :: Var -> a -> b -> b

  inEnv :: Env a -> b -> b
  inEnv env e = foldl' (flip $ uncurry sub) e (Map.toList env)

class Contextual a where
  fv :: a -> Set.Set Var
  bv :: a -> Set.Set Var
  fvOf :: a -> Var -> Bool
  fvOf = flip Set.member . fv

instance Substitutable a b => Substitutable a (Pair b) where
  sub :: Substitutable a b => Var -> a -> Pair b -> Pair b
  sub v a (b0, b1) = (sub v a b0, sub v a b1)

instance Substitutable a b => Substitutable a [b] where
  sub :: Substitutable a b => Var -> a -> [b] -> [b]
  sub v a = map (sub v a)

instance Substitutable a b => Substitutable a (Env b) where
  sub :: Substitutable a b => Var -> a -> Env b -> Env b
  sub v a = Map.map (sub v a)

instance Contextual a => Contextual (Pair a) where
  fv :: Contextual a => (a, a) -> Set.Set Var
  fv (t0, t1) = fv t0 `Set.union` fv t1

unfoldSet :: (a -> Set.Set Var) -> [a] -> Set.Set Var
unfoldSet f = foldr (Set.union . f) Set.empty

instance {-# OVERLAPPABLE #-} Contextual a => Contextual [a] where
  fv :: [a] -> Set.Set Var
  fv = unfoldSet fv

  bv :: Contextual a => [a] -> Set.Set Var
  bv = unfoldSet bv

instance Contextual a => Contextual (Env a) where
  fv :: Contextual a => Env a -> Set.Set Var
  fv env = fv $ Map.elems env

(<.>) :: Substitutable a a => Env a -> Env a -> Env a
(<.>) e0 e1 = Map.map (inEnv e0) e1 `Map.union` e0

type FreshT m = StateT Int m

type FreshM a = FreshT Identity a

fresh' :: Num b => b -> (b, b)
fresh' s = (s + 1, s + 1)

fresh :: Monad m => FreshT m Int
fresh = state fresh'

next' :: Var -> FreshM Var
next' (Var vPub _) = Var vPub . show <$> fresh

evalFreshT :: Int -> FreshT Identity a -> a
evalFreshT s m = runIdentity $ evalStateT m s

runFreshT :: Int -> FreshT Identity a -> (a, Int)
runFreshT s m = runIdentity $ runStateT m s

-- | A renamable `a` conforms to the barendregt convention.
class Contextual a => Renamable a where
  rename' :: Set.Set Var -> a -> FreshM a

  next :: Var -> FreshM a

  rename :: a -> FreshM a
  rename a = rename' (fv a) a

  evalRename' :: FreshM a -> a
  evalRename' = evalFreshT 0

  evalRename :: a -> a
  evalRename a = evalRename' (rename a)

  runRename' :: FreshM a -> (a, Int)
  runRename' = runFreshT 0

  runRename :: a -> (a, Int)
  runRename a = runRename' (rename a)

-- | Expressions should be reduced before checking
--   for alpha equivalence.
class AlphaComparable a where
  (@=) :: Eq a => a -> a -> Bool

  (@!=) :: Eq a => a -> a -> Bool
  e0 @!= e1 = not (e0 @= e1)

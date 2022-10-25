{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Nat.Context where

import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.State (StateT, evalStateT, runStateT, state)
import Data.Foldable (foldl')
import qualified Data.Map as Map
import qualified Data.Set as Set
import Mean.Viz
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
  (Var _ v) == (Var _ v') = v == v'

instance Ord Var where
  compare (Var _ v0) (Var _ v1) = compare v0 v1

instance Show Var where
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
  sub v a (b0, b1) = (sub v a b0, sub v a b1)

instance Substitutable a b => Substitutable a [b] where
  sub v a = map (sub v a)

instance Substitutable a b => Substitutable a (Env b) where
  sub v a = Map.map (sub v a)

instance Contextual a => Contextual (Pair a) where
  fv (t0, t1) = fv t0 `Set.union` fv t1

instance {-# OVERLAPPABLE #-} Contextual a => Contextual [a] where
  fv = foldr (Set.union . fv) Set.empty
  bv = foldr (Set.union . bv) Set.empty

instance Contextual a => Contextual (Env a) where
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

class Contextual a => Renamable a where
  rename' :: Set.Set Var -> a -> FreshM a

  next :: Var -> FreshM a

  rename :: a -> FreshM a
  rename expr = rename' (fv expr) expr

  evalRename' :: FreshM a -> a
  evalRename' = evalFreshT 0

  evalRename :: a -> a
  evalRename a = evalRename' (rename a)

  runRename' :: FreshM a -> (a, Int)
  runRename' = runFreshT 0

  runRename :: a -> (a, Int)
  runRename a = runRename' (rename a)

  alphaEq :: Eq a => a -> a -> Bool
  alphaEq e0 e1 = evalRename e0 == evalRename e1

  (@=) :: Eq a => a -> a -> Bool
  (@=) = alphaEq

  (@!=) :: Eq a => a -> a -> Bool
  e0 @!= e1 = not (e0 @= e1)

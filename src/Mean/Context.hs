{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Mean.Context where

import Control.Monad (replicateM)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.State (MonadState (get, put), StateT, evalStateT)
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
  (==) :: Var -> Var -> Bool
  (Var _ v) == (Var _ v') = v == v'

instance Ord Var where
  compare :: Var -> Var -> Ordering
  compare (Var _ v0) (Var _ v1) = compare v0 v1

instance Show Var where
  show :: Var -> String
  show (Var vPub vPri) = vPri

instance Pretty [Var] where
  ppr p (v : vs) = text (show v) <> char ',' <> text (show v)
  ppr p [] = text ""

mkVar :: Name -> Var
mkVar v = Var v v

mkEnv :: Var -> a -> Env a
mkEnv = Map.singleton

class Substitutable a b where
  substitute :: Env a -> b -> b

  inEnv :: Var -> a -> b -> b
  inEnv v a = substitute (mkEnv v a)

class Contextual a where
  fv :: a -> Set.Set Var

instance Substitutable a b => Substitutable a (Pair b) where
  substitute :: Substitutable a b => Env a -> Pair b -> Pair b
  substitute s (a0, a1) = (substitute s a0, substitute s a1)

instance Substitutable a b => Substitutable a [b] where
  substitute :: Env a -> [b] -> [b]
  substitute = map . substitute

instance Substitutable a b => Substitutable a (Env b) where
  substitute :: Substitutable a b => Env a -> Env b -> Env b
  substitute s env = Map.map (substitute s) env

instance Contextual a => Contextual (Pair a) where
  fv :: Contextual a => (a, a) -> Set.Set Var
  fv (t0, t1) = fv t0 `Set.union` fv t1

instance Contextual a => Contextual [a] where
  fv :: [a] -> Set.Set Var
  fv = foldr (Set.union . fv) Set.empty

instance Contextual a => Contextual (Env a) where
  fv :: Contextual a => Env a -> Set.Set Var
  fv env = fv $ Map.elems env

(<.>) :: Substitutable a a => Env a -> Env a -> Env a
(<.>) e0 e1 = Map.map (substitute e0) e1 `Map.union` e0

type RenameM a = StateT Int Identity a

next :: Var -> RenameM Var
next (Var vPub _) = do
  s <- get
  put (s + 1)
  pure $ Var vPub (show s)

class (Contextual a, Substitutable a a) => Renamable a where
  rename :: a -> RenameM a
  runRename :: a -> a
  runRename a = runIdentity $ evalStateT (rename a) 0

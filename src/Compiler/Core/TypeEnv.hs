{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Compiler.Core.TypeEnv
  ( Env (..),
    empty,
    lookup,
    remove,
    extend,
    extends,
    merge,
    mergeEnvs,
    singleton,
    keys,
    fromList,
    toList,
    isIn,
  )
where

import qualified Compiler.Core.Syntax as Syn
import Data.Foldable hiding (toList)
import qualified Data.Map as Map
import Data.Monoid
import Prelude hiding (lookup)

-------------------------------------------------------------------------------
-- Typing Environment
-------------------------------------------------------------------------------

newtype Env = TypeEnv {types :: Map.Map Syn.Name Syn.TyScheme}
  deriving (Eq)

empty :: Env
empty = TypeEnv Map.empty

extend :: Env -> (Syn.Name, Syn.TyScheme) -> Env
extend env (x, s) = env {types = Map.insert x s (types env)}

remove :: Env -> Syn.Name -> Env
remove (TypeEnv env) var = TypeEnv (Map.delete var env)

extends :: Env -> [(Syn.Name, Syn.TyScheme)] -> Env
extends env xs = env {types = Map.union (Map.fromList xs) (types env)}

lookup :: Syn.Name -> Env -> Maybe Syn.TyScheme
lookup key (TypeEnv tys) = Map.lookup key tys

merge :: Env -> Env -> Env
merge (TypeEnv a) (TypeEnv b) = TypeEnv (Map.union a b)

mergeEnvs :: [Env] -> Env
mergeEnvs = foldl' merge empty

singleton :: Syn.Name -> Syn.TyScheme -> Env
singleton x y = TypeEnv (Map.singleton x y)

keys :: Env -> [Syn.Name]
keys (TypeEnv env) = Map.keys env

isIn :: Syn.TyScheme -> Env -> Bool
isIn ty = elem ty . map snd . toList

fromList :: [(Syn.Name, Syn.TyScheme)] -> Env
fromList xs = TypeEnv (Map.fromList xs)

toList :: Env -> [(Syn.Name, Syn.TyScheme)]
toList (TypeEnv env) = Map.toList env

instance Semigroup Env where
  (<>) = merge

instance Monoid Env where
  mempty = empty

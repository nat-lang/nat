{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Nat.Context where

import Data.Fix
import qualified Data.Map as Map
import qualified Data.Set as Set
import Mean.Viz
import Nat.Walk (Walkable (walkC))
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

var :: Name -> Var
var v = Var v v

class Functor f => Contextual f where
  fv' :: f (Set.Set Var) -> Set.Set Var

  fv :: Fix f -> Set.Set Var
  fv = foldFix fv'

class (Walkable (Fix f), Functor f) => Substitutable f where
  sub' :: (Fix f -> Fix f) -> Var -> Fix f -> Fix f -> Fix f

  sub :: Var -> Fix f -> Fix f -> Fix f
  sub v e = walkC $ \e' c -> sub' c v e e'

{-
bus :: Var -> f (Fix f) -> Fix f -> (Fix f -> r) -> r
bus v e = anaC (bus' v e)

bus' :: Var -> f (Fix f) -> Fix f -> (f (Fix f) -> r) -> r
-}

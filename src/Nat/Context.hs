{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Nat.Context where
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

class Substitutable a b where
  sub' :: (b -> b) -> Var -> a -> b -> b

  sub :: Walkable b => Var -> a -> b -> b
  sub v e = walkC $ \e' c -> sub' c v e e'

class Contextual a where
  fv :: a -> Set.Set Var

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Mean.Var where

import Mean.Viz
import Text.PrettyPrint
  ( char,
    text,
    (<>),
  )
import Prelude hiding ((<>))

type Name = String

data Var = Var !Name !Name

instance Eq Var where
  (==) :: Var -> Var -> Bool
  (Var _ v) == (Var _ v') = v == v'

instance Ord Var where
  compare :: Var -> Var -> Ordering
  compare (Var _ v0) (Var _ v1) = compare v0 v1

instance Show Var where
  show :: Var -> String
  show (Var vPub vPri) = vPub

instance Pretty [Var] where
  ppr p (v : vs) = text (show v) <> char ',' <> text (show v)
  ppr p [] = text ""

mkVar :: Name -> Var
mkVar v = Var v (v ++ "0")

{-# LANGUAGE GADTs #-}

module Nat.Syntax.Type where

import Nat.Cata
import Nat.Context

data AType a where
  TyVar :: Var -> AType a
  TyCon :: String -> AType a
  TyFun :: a -> a -> AType a
  deriving (Eq, Ord)

type Type = Mu AType
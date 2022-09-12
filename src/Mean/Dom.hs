{-# LANGUAGE GADTs #-}

module Mean.Dom where

import Mean.Core
import Mean.Set
import Mean.Syntax

data DomExpr a where
  Dom :: Expressible a => Type -> Set a -> DomExpr a

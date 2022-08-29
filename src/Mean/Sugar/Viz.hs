{-# LANGUAGE FlexibleInstances #-}

module Mean.Sugar.Viz where

import Mean.Core.Viz
import Mean.Sugar.Syntax
import Text.PrettyPrint
import Prelude hiding ((<>))


instance Pretty Expr where
  ppr p e = case e of
    ETree t -> text $ show t

instance Show Expr where
  show (ETree et) = show et


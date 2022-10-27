{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Mean.Syntax.Logic where

import qualified Data.Set as Set
import Mean.Context (Var)
import Mean.Viz
import Text.PrettyPrint
  ( Doc,
    text,
    (<+>),
    (<>),
  )
import Prelude hiding ((<>))

data QExpr v a
  = Univ !v !a
  | Exis !v !a
  deriving (Eq, Ord, Show)

pq :: (Pretty p, Pretty a) => Int -> a -> p -> Doc
pq p v b = brackets (ppr p v) <> ":" <+> ppr p b

instance (Pretty a, Pretty v) => Pretty (QExpr v a) where
  ppr :: Int -> QExpr v a -> Doc
  ppr p qe = case qe of
    Univ v b -> "Forall" <+> pq p v b
    Exis v b -> "Exists" <+> pq p v b

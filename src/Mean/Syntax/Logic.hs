{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Mean.Syntax.Logic where

import qualified Data.Set as Set
import Mean.Var (Var)
import Mean.Viz
import Text.PrettyPrint
  ( Doc,
    text,
    (<+>),
    (<>),
  )
import Prelude hiding ((<>))

data QExpr a
  = Univ ![Var] !a
  | Exis ![Var] !a
  deriving (Eq, Ord, Show)

pq :: (Pretty p, Pretty [a]) => Int -> [a] -> p -> Doc
pq p vs b = brackets (ppr p vs) <> ":" <+> ppr p b

instance Pretty a => Pretty (QExpr a) where
  ppr :: Pretty a => Int -> QExpr a -> Doc
  ppr p qe = case qe of
    Univ vs b -> "Forall" <+> pq p vs b
    Exis vs b -> "Exists" <+> pq p vs b

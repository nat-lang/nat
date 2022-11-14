{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Nat.TeX where

import Data.List (intersperse)
import qualified Data.Text as T
import Data.Tree.Binary.Preorder (Tree (..))
import Nat.Syntax.Module
import Nat.Syntax.Surface
import Nat.Walk
import Text.LaTeX
import Text.LaTeX.Base
import Text.LaTeX.Base.Class
import Text.LaTeX.Packages.Inputenc
import Text.LaTeX.Packages.Trees.Qtree (qtree)

class TeX a where
  toTex :: a -> LaTeXM T.Text

  typeset :: a -> T.Text
  typeset e = render (execLaTeXM doc)
    where
      doc :: LaTeXM T.Text
      doc = do
        documentclass [] article
        usepackage [] qtree
        usepackage [utf8] inputenc
        document <$> tex e

instance TeX a => TeX (Tree a) where
  toTex = \case
    Leaf -> mempty
    Node e l r ->
      mconcat
        [ "[",
          (("." <>) . braces . toTex) e,
          " ",
          mconcat $ intersperse " " $ fmap toTex [l, r],
          " ]"
        ]

instance TeX Expr where
  toTex = walk $ \case
    ETree t -> toTex t
    e -> TexRaw (T.pack $ show e)
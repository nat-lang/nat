{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Nat.TeX (render, toTeX, typeset, pp, typesetFile) where

import Data.List (intersperse)
import qualified Data.Text as T
import Data.Tree.Binary.Preorder (Tree (..))
import Nat.Syntax.Module
import Nat.Syntax.Surface
import Nat.Walk
import Text.LaTeX
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Pretty
import Text.LaTeX.Base.Syntax (LaTeX (..))
import Text.LaTeX.Packages.Inputenc
import Text.LaTeX.Packages.Trees.Qtree (qtree)

pp = T.pack . prettyLaTeX

ppf f = renderFile f . pp

class TeX a where
  toTeX :: a -> LaTeX

  typeset :: a -> LaTeX
  typeset e =
    documentclass [] article
      <> usepackage [] qtree
      <> usepackage [utf8] inputenc
      <> document (toTeX e)

  typesetFile :: FilePath -> a -> IO ()
  typesetFile f = renderFile f . typeset

instance TeX a => TeX (Tree a) where
  toTeX = \case
    Leaf -> mempty
    Node e l r ->
      mconcat
        [ "[",
          (("." <>) . braces . toTeX) e,
          " ",
          mconcat $ intersperse " " $ fmap toTeX [l, r],
          "]"
        ]

instance TeX Expr where
  toTeX = \case
    ETree t -> commS "Tree" <> " " <> toTeX t
    e -> TeXRaw (T.pack $ show e)

instance TeX ModuleExpr where
  toTeX = foldl1 (<>) . fmap toTeX

instance TeX Module where
  toTeX = mconcat . intersperse "\n" . fmap toTeX

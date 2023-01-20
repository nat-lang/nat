{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Nat.TeX (render, toTeX, typeset, pp, typesetFile) where

import Data.List (intersperse)
import qualified Data.Text as T
import Data.Tree.Binary.Preorder (Tree (..))
import Nat.Context
import Nat.Syntax.Module
import Nat.Syntax.Surface
import Nat.Walk
import Text.LaTeX
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Pretty
import Text.LaTeX.Base.Syntax (LaTeX (..))
import Text.LaTeX.Packages.Inputenc
import Text.LaTeX.Packages.TikZ hiding (Node)

pp = T.pack . prettyLaTeX

ppf f = renderFile f . pp

tikzQTree :: PackageName
tikzQTree = "tikz-qtree"

tikzQTreeCompat :: PackageName
tikzQTreeCompat = "tikz-qtree-compat"

class TeX a where
  toTeX :: a -> LaTeX

  typeset :: a -> LaTeX
  typeset e =
    documentclass [] article
      -- <> usepackage [] qtree
      <> usepackage [] tikz
      <> usepackage [] tikzQTree
      <> usepackage [] tikzQTreeCompat
      <> usepackage [utf8] inputenc
      <> document (TeXEnv "tikzpicture" [] (toTeX e))

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

traw :: (Show a) => a -> LaTeX
traw = TeXRaw . T.pack . show

str = TeXRaw . T.pack

(<+>) :: LaTeX -> LaTeX -> LaTeX
(<+>) l l' = between (str " ") l l'

instance TeX Var where
  toTeX = traw

instance TeX a => TeX (Binder a) where
  toTeX (Binder v _) = str "\\lambda" <+> toTeX v

instance TeX Lit where
  toTeX = traw

instance TeX Expr where
  toTeX = \case
    ETree t -> commS "Tree" <+> toTeX t
    ELit l -> toTeX l
    EVar v -> traw v
    EBind b -> toTeX b
    ELam b e -> math $ toTeX b <+> autoBrackets (str "[") (str "]") (toTeX e)
    EApp e e' -> toTeX e <> autoParens (toTeX e')
    {-
    ECond Expr Expr Expr ->
    EUnOp UnOp Expr ->
    EBinOp BinOp Expr Expr ->
    ETree (T.Tree Expr) ->
    ELitCase Expr [(Expr, Expr)] ->
    ETyCase Expr [(Binder Expr, Expr)] ->
    ESet (Set Expr) ->
    EFix Var Expr ->
    ETup [Expr] ->
    -- EIdx Int
    EWild ->
    EUndef ->
    EDom (Domain Expr) ->
    EQnt (QExpr Expr) ->
      -}
    e -> TeXRaw (T.pack $ show e)

instance TeX ModuleExpr where
  toTeX = foldl1 (<>) . fmap toTeX

instance TeX Module where
  toTeX = mconcat . intersperse "\n" . fmap toTeX

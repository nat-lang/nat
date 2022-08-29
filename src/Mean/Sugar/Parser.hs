{-# LANGUAGE OverloadedStrings #-}

module Mean.Sugar.Parser where

import qualified Mean.Sugar.Syntax as S
import qualified Text.Megaparsec as P
import Text.Megaparsec ((<|>))
import Data.Functor ((<&>))
import qualified Mean.Common.Lexer as L
import qualified Mean.Core.Parser as Core

type ExprParser = L.Parser S.Expr
type ExprTreeParser = L.Parser S.ExprTree

pTree :: ExprTreeParser
pTree = P.try (L.brackets unaryCatNode) <|> L.brackets binaryCatNode
  where
    mkLeafNode l = S.Node l S.Leaf S.Leaf
    mkUnaryNode l c = S.Node l c S.Leaf

    node = P.try leafNode <|> P.try (L.brackets leafNode) <|> pTree

    leafNode :: ExprTreeParser
    leafNode = do mkLeafNode <$> Core.pExpr

    unaryCatNode :: ExprTreeParser
    unaryCatNode = do
      l <- Core.pExpr
      mkUnaryNode l <$> node

    binaryCatNode :: ExprTreeParser
    binaryCatNode = do
      l <- Core.pExpr
      c <- node
      S.Node l c <$> node

pInterpretation :: ExprParser
pInterpretation = do
  L.reserved "interpret"
  S.ETree <$> pTree

pExpr :: ExprParser
pExpr = P.choice
  [ Core.pExpr <&> S.ECore,
    pTree <&> S.ETree
  ]
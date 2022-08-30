{-# LANGUAGE OverloadedStrings #-}

module Mean.Sugar.Parser where

import Control.Applicative (some)
import Data.Functor ((<&>))
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import qualified Mean.Common.Lexer as L
import qualified Mean.Core.Parser as Core
import qualified Mean.Core.Syntax as SCore
import qualified Mean.Sugar.Syntax as S
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as P

type ExprParser = L.Parser S.SugarExpr

type ExprTreeParser = L.Parser S.ExprTree

pExprNode :: L.Parser S.ExprNode
pExprNode = P.choice
  [ Core.pCExpr <&> S.ENode,
    Core.pBinder <&> S.BNode
  ]

pTree :: ExprTreeParser
pTree = P.try (L.brackets unNode) <|> L.brackets biNode
  where
    mkLeafNode e = S.Node e S.Leaf S.Leaf
    mkUnNode e l = S.Node e l S.Leaf

    node = P.try leafNode <|> P.try (L.brackets leafNode) <|> pTree

    leafNode :: ExprTreeParser
    leafNode = mkLeafNode <$> pExprNode

    unNode :: ExprTreeParser
    unNode = do
      e <- pExprNode
      mkUnNode e <$> node

    biNode :: ExprTreeParser
    biNode = do
      e <- pExprNode
      l <- node
      S.Node e l <$> node

pSLit :: ExprParser
pSLit = S.SLit <$> Core.pLit

pSVar :: ExprParser
pSVar = S.SVar <$> Core.pVar

pSTree :: ExprParser
pSTree = S.STree <$> pTree

pSLam :: ExprParser
pSLam = do
  lam <- Core.pLam
  S.SLam . lam <$> pSExpr

sTerms =
  [ L.parens pSExpr,
    pSLit,
    pSVar,
    pSTree,
    pSLam
  ]

pSTerm :: ExprParser
pSTerm = P.choice sTerms

operatorTable :: [[Operator L.Parser S.SugarExpr]]
operatorTable =
  [ [],
    -- prefix "-" Negation,
    -- prefix "+" id

    [],
    -- infix "*" Product,
    -- infix "/" Division

    []
  ]

-- infix "+" Sum,
-- infix "-" Subtr

pSExpr' :: ExprParser
pSExpr' = makeExprParser pSTerm operatorTable

pSExpr :: ExprParser
pSExpr = do
  exprs <- some pSExpr'
  pure (foldl1 (\e0 e1 -> S.SApp $ SCore.App e0 e1) exprs)

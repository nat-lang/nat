{-# LANGUAGE OverloadedStrings #-}

module Mean.Sugar.Parser where

import Control.Applicative (some)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Functor ((<&>))
import Debug.Trace (traceM)
import qualified Mean.Common.Lexer as L
import qualified Mean.Core.Parser as Core
import qualified Mean.Core.Syntax as SCore
import qualified Mean.Sugar.Syntax as S
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as P

type ExprParser = L.Parser S.SugarExpr

type ExprTreeParser = L.Parser S.ExprTree

pCExpr = P.try Core.pCExpr <|> Core.pCBind

mkLeafNode e = S.Node e S.Leaf S.Leaf

mkUnNode e l = S.Node e l S.Leaf

pLeafNode :: ExprTreeParser
pLeafNode = L.brackets $ mkLeafNode <$> pCExpr

pUnNode :: ExprTreeParser
pUnNode = L.brackets $ do
  e <- pCExpr
  mkUnNode e <$> pTree

pBiNode :: ExprTreeParser
pBiNode = L.brackets $ do
  e <- pCExpr
  l <- pTree
  S.Node e l <$> pTree

pTree :: ExprTreeParser
pTree =
  P.choice
    [P.try pLeafNode, P.try pUnNode, pBiNode]

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

pSTerm :: ExprParser
pSTerm =
  P.choice
    [ L.parens pSExpr,
      pSLit,
      pSVar,
      pSTree,
      pSLam
    ]

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

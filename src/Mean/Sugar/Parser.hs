{-# LANGUAGE OverloadedStrings #-}

module Mean.Sugar.Parser where

import Control.Applicative (some)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Control.Monad.State
  ( MonadState (get, put),
  )
import Data.Functor ((<&>))
import Debug.Trace (traceM)
import qualified Mean.Common.Lexer as L
import qualified Mean.Core.Parser as Core
import qualified Mean.Core.Syntax as SCore
import qualified Mean.Sugar.Syntax as S
import Text.Megaparsec ((<|>))
import Text.Megaparsec.Debug (dbg)
import qualified Text.Megaparsec as P
import Mean.Sugar.Viz

type ExprParser = L.Parser S.SugarExpr

type ExprTreeParser = L.Parser S.ExprTree

pENode = P.try pSExpr <|> pSBind

mkLeafNode e = S.Node e S.Leaf S.Leaf

mkUnNode e l = S.Node e l S.Leaf

pLeafNode :: ExprTreeParser
pLeafNode = L.brackets $ mkLeafNode <$> pENode

pUnNode :: ExprTreeParser
pUnNode = L.brackets $ do
  e <- pENode
  mkUnNode e <$> pTree

pBiNode :: ExprTreeParser
pBiNode = L.brackets $ do
  e <- pENode
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

finally fn p = do
  mA <- P.observing p
  case mA of
    Left e -> fn >> P.parseError e
    Right a -> fn >> pure a

pSTree :: ExprParser
pSTree = do
  s <- get
  put (s {L.inTree = True})
  t <- finally (put $ s {L.inTree = False}) pTree
  pure $ S.STree t

pSBind :: ExprParser
pSBind = S.SBind <$> Core.pBinder

pSLam :: ExprParser
pSLam = do
  lam <- Core.pLam
  S.SLam . lam <$> pSExpr

sTerms =
  [ L.parens pSExpr,
    pSLit,
    pSVar,
    pSLam
  ]

pSTerm :: ExprParser
pSTerm = do
  s <- get
  P.choice $
    [pSTree | not (L.inTree s)] ++ sTerms

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

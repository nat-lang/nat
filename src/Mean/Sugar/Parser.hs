{-# LANGUAGE OverloadedStrings #-}

module Mean.Sugar.Parser where

import Control.Applicative (some)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Control.Monad.State
  ( MonadState (get, put),
  )
import Debug.Trace (traceM)
import qualified Mean.Common.Lexer as L
import qualified Mean.Core.Parser as Core
import qualified Mean.Core.Syntax as CSyn
import qualified Mean.Sugar.Syntax as S
import Mean.Sugar.Viz
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as Lex
import Text.Megaparsec.Debug (dbg)

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

pSCond = do
  c <- Core.pCond pSExpr
  pure $ c S.STernOp

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

pSet = L.curlies (pSExpr `P.sepBy` (L.space >> C.char ',' >> L.space))

pSSet = S.SSet <$> pSet

pCase = do
  c <- pSExpr
  L.reserved ":"
  r <- pSExpr
  pure (c, r)

pSCase = P.try $ Lex.indentBlock L.spaceN p
  where
    p = do
      L.reserved "case"
      base <- pSExpr
      L.reserved "of"
      pure $ Lex.IndentSome Nothing (pure . S.SCase base) pCase

sTerms =
  [ L.parens pSExpr,
    pSLit,
    pSVar,
    pSLam,
    pSCond,
    pSCase,
    pSSet
  ]

pSTerm :: ExprParser
pSTerm = do
  s <- get
  P.choice $
    [pSTree | not (L.inTree s)] ++ sTerms

operatorTable :: [[Operator L.Parser S.SugarExpr]]
operatorTable =
  [ [ L.prefixOp "!" (S.SUnOp CSyn.Neg)
    ],
    [ L.infixOpL "==" (S.SBinOp CSyn.Eq),
      L.infixOpL "!=" (S.SBinOp CSyn.NEq),
      L.infixOpL "&&" (S.SBinOp CSyn.And),
      L.infixOpL "||" (S.SBinOp CSyn.Or)
    ]
  ]

pSExpr' :: ExprParser
pSExpr' = makeExprParser pSTerm operatorTable

pSExpr :: ExprParser
pSExpr = do
  exprs <- some pSExpr'
  pure (foldl1 (\e0 e1 -> S.SApp $ CSyn.App e0 e1) exprs)

{-# LANGUAGE OverloadedStrings #-}

module Mean.Parser where

import Control.Applicative (some)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Functor ((<&>))
import Data.Text (Text)
import Mean.Lexer as L
import Mean.Syntax as S
import Mean.Typing as T
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as P

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

type TyParser = Parser T.Type

pTyTerm :: TyParser
pTyTerm =
  P.choice
    [ L.angles pType,
      reserved "t" >> pure T.tyBool,
      reserved "n" >> pure T.tyInt,
      L.titularIdentifier <&> (T.TyVar . T.TV),
      L.identifier <&> T.TyCon
    ]

tyNil :: TyParser
tyNil = pure T.TyNil

pType :: TyParser
pType = makeExprParser pTyTerm tyOps
  where
    tyOps =
      [ [infixOpR "," T.TyFun]
      ]

pTypeAssignment :: TyParser
pTypeAssignment = (reserved ":" >> pType) <|> tyNil

pOptionalTypeAssignment :: TyParser
pOptionalTypeAssignment = pTypeAssignment <|> tyNil

-------------------------------------------------------------------------------
-- Terms
-------------------------------------------------------------------------------

type ExprParser = L.Parser S.Expr

pBool :: ExprParser
pBool =
  (L.reserved "True" >> pure (S.ELit (S.LBool True)))
    <|> (L.reserved "False" >> pure (S.ELit (S.LBool False)))

pInt :: ExprParser
pInt = S.ELit . S.LInt . fromIntegral <$> integer

pVar :: ExprParser
pVar = S.Var <$> L.identifier

pBinder :: Parser S.Binder
pBinder = do
  n <- L.identifier
  S.Binder n <$> pOptionalTypeAssignment

pLam :: ExprParser
pLam = do
  L.symbol "\\"
  b <- pBinder
  L.symbol "."
  S.Lam b <$> pExpr

pTerm :: ExprParser
pTerm =
  P.choice
    [ L.parens pExpr,
      pInt,
      pBool,
      pVar,
      pLam
    ]

operatorTable :: [[Operator Parser Expr]]
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

pExpr' :: ExprParser
pExpr' = makeExprParser pTerm operatorTable

pExpr :: ExprParser
pExpr = do
  exprs <- some pExpr'
  pure (foldl1 S.App exprs)

-------------------------------------------------------------------------------
-- Trees
-------------------------------------------------------------------------------

type ExprTreeParser = L.Parser S.ExprTree

pTree :: ExprTreeParser
pTree = P.try (brackets unaryCatNode) <|> brackets binaryCatNode
  where
    mkLeafNode l = S.Node l S.Leaf S.Leaf
    mkUnaryNode l c = S.Node l c S.Leaf

    node = P.try leafNode <|> P.try (brackets leafNode) <|> pTree

    leafNode :: ExprTreeParser
    leafNode = do mkLeafNode <$> pExpr

    unaryCatNode :: ExprTreeParser
    unaryCatNode = do
      l <- pExpr
      mkUnaryNode l <$> node

    binaryCatNode :: ExprTreeParser
    binaryCatNode = do
      l <- pExpr
      c <- node
      S.Node l c <$> node

-------------------------------------------------------------------------------
-- Entrypoints
-------------------------------------------------------------------------------

parse parser = P.runParser parser "<input"
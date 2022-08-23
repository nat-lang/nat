{-# LANGUAGE OverloadedStrings #-}

module Mean.Parser
  ( parseTree,
  )
where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Text (Text)
import Mean.Lexer as L
import Mean.Syntax as S
import Mean.Typing as T
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as P

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

tylit :: Parser S.Type
tylit =
  (reservedOp "t" >> pure S.tyBool)
    <|> (reservedOp "n" >> pure S.tyInt)
    <|> (titularIdentifier <&> (S.TyVar . S.TV))
    <|> (identifier <&> S.TyCon)

tyatom :: Parser S.Type
tyatom = tylit <|> angles parseType

isTyVar ty = case ty of
  S.TyVar _ -> True
  _ -> False

tyNil :: Parser S.Type
tyNil = do
  pure S.TyNil

parseType :: Parser S.Type
parseType = Ex.buildExpressionParser tyops tyatom
  where
    infixOp x f = Ex.Infix (reservedOp x >> pure f)
    tyops =
      [ [infixOp "," S.TyFun Ex.AssocRight]
      ]

parseTypeAssignment :: Parser S.Type
parseTypeAssignment = (reservedOp ":" >> parseType) <|> tyNil

parseOptionalTypeAssignment :: Parser S.Type
parseOptionalTypeAssignment = parseTypeAssignment <|> tyNil

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
  n <- identifier
  S.Binder n <$> parseOptionalTypeAssignment

pLam :: ExprParser
pLam = do
  reserved "\\"
  b <- parseBinder
  reserved "."
  S.Lam b <$> parseExpr'

pApp :: ExprParser
pApp = do
  exprs <- many1 pExpr
  pure (foldl1 S.App exprs)

pTerm :: ExprParser
pTerm =
  P.choice
    [ pInt,
      pBool,
      pVar,
      pLam,
      pApp
    ]

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [],
    -- prefix "-" Negation,
    -- prefix "+" id

    [],
    -- binary "*" Product,
    -- binary "/" Division

    []
  ]

-- binary "+" Sum,
-- binary "-" Subtr

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ L.symbol name)

prefix, postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix name f = Prefix (f <$ L.symbol name)
postfix name f = Postfix (f <$ L.symbol name)

pExpr :: ExprParser
pExpr = makeExprParser pTerm operatorTable

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

parseTree = P.runParser pTree "<input>"

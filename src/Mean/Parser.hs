{-# LANGUAGE OverloadedStrings #-}

module Mean.Parser
  ( P.ParseError (..),
    pLet,
    pExpr,
    pVar,
    pInt,
    pBool,
    pTree,
    pLam,
    pType,
    pModule,
    pFModule,
    parse,
  )
where

import Control.Applicative (some)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Functor ((<&>))
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text.IO as TiO
import Data.Void
import qualified Mean.Lexer as L
import qualified Mean.Syntax as S
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as C

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

type TyParser = L.Parser S.Type

pTyTerm :: TyParser
pTyTerm =
  P.choice
    [ L.angles pType,
      L.reserved "t" >> pure S.tyBool,
      L.reserved "n" >> pure S.tyInt,
      L.titularIdentifier <&> (S.TyVar . S.TV),
      L.identifier <&> S.TyCon
    ]

tyNil :: TyParser
tyNil = pure S.TyNil

pType :: TyParser
pType = makeExprParser pTyTerm tyOps
  where
    tyOps =
      [ [L.infixOpR "," S.TyFun]
      ]

pTypeAssignment :: TyParser
pTypeAssignment = (L.reserved ":" >> pType) <|> tyNil

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
pInt = S.ELit . S.LInt . fromIntegral <$> L.integer

pVar :: ExprParser
pVar = do
  S.mkEVar <$> L.identifier

pBinder :: L.Parser S.Binder
pBinder = do
  n <- L.identifier
  S.Binder (S.mkVar n) <$> pOptionalTypeAssignment

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

operatorTable :: [[Operator L.Parser S.Expr]]
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
-- Modules
-------------------------------------------------------------------------------

pLet :: ExprParser
pLet = do
  L.reserved "let"
  name <- L.identifier
  L.space
  L.symbol "="
  L.space
  S.Let name <$> pExpr

pModule :: L.Parser [S.Expr]
pModule = pLet `P.sepBy` L.delimiter

-------------------------------------------------------------------------------
-- Trees
-------------------------------------------------------------------------------

type ExprTreeParser = L.Parser S.ExprTree

pTree :: ExprTreeParser
pTree = P.try (L.brackets unaryCatNode) <|> L.brackets binaryCatNode
  where
    mkLeafNode l = S.Node l S.Leaf S.Leaf
    mkUnaryNode l c = S.Node l c S.Leaf

    node = P.try leafNode <|> P.try (L.brackets leafNode) <|> pTree

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

type ModuleParse = Either (P.ParseErrorBundle Text Data.Void.Void) [S.Expr]

parse :: P.Parsec e s a -> s -> Either (P.ParseErrorBundle s e) a
parse parser = P.runParser parser "<input>"

parseFile :: P.Parsec e Text a -> String -> IO (Either (P.ParseErrorBundle Text e) a)
parseFile parser file = P.runParser parser file <$> TiO.readFile file

pFModule :: String -> IO ModuleParse
pFModule = parseFile pModule

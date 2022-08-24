module Compiler.Core.Parser
  ( parseExpr,
    parseFrag,
    parseFragS,
    parseDecl,
    ParseError,
  )
where

import Compiler.Core.Fresh (letters)
import Compiler.Core.Lexer
import Compiler.Core.Pretty
import qualified Compiler.Core.Syntax as Syn
import Control.Monad (void)
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity)
import qualified Data.Map as Map
import Debug.Trace (trace, traceM)
import System.IO (FilePath, IO)
import Text.Parsec
import qualified Text.Parsec.Expr as Ex
import Text.ParserCombinators.Parsec.Combinator (choice)

debugParse :: String -> Parser a -> Parser a
debugParse s p =
  if False
    then tryParse s *> p <* completeParse s
    else p
  where
    tryParse p = parserTrace ("parsing " ++ p ++ "...")
    completeParse p = parserTrace ("ok, parsed " ++ p)

titularIdentifier = lookAhead upper >> identifier

lIdentifier = lookAhead lower >> identifier

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

tylit :: Parser Syn.Type
tylit =
  (reservedOp "t" >> pure Syn.tyBool)
    <|> (reservedOp "n" >> pure Syn.tyInt)
    <|> (titularIdentifier <&> (Syn.TyVar . Syn.TV))
    <|> (identifier <&> Syn.TyCon)

tyatom :: Parser Syn.Type
tyatom = tylit <|> angles parseType

tyNil :: Parser Syn.Type
tyNil = do
  pure Syn.TyNil

parseType :: Parser Syn.Type
parseType = Ex.buildExpressionParser tyops tyatom
  where
    infixOp x f = Ex.Infix (reservedOp x >> pure f)
    tyops =
      [ [infixOp "," Syn.TyFun Ex.AssocRight]
      ]

-------------------------------------------------------------------------------
-- Expressions
-------------------------------------------------------------------------------

parseTypeAssignment :: Parser Syn.Type
parseTypeAssignment = (reservedOp ":" >> parseType) <|> tyNil

parseOptionalTypeAssignment :: Parser Syn.Type
parseOptionalTypeAssignment = parseTypeAssignment <|> tyNil

parseBool :: Parser Syn.Expr
parseBool =
  debugParse "bool" (reserved "True" >> pure (Syn.ELit (Syn.LBool True)))
    <|> (reserved "False" >> pure (Syn.ELit (Syn.LBool False)))

parseNumber :: Parser Syn.Expr
parseNumber = debugParse "number" $ do
  Syn.ELit . Syn.LInt . fromIntegral <$> natural

parseVar :: Parser Syn.Expr
parseVar = debugParse "var" $ do
  Syn.Var <$> lIdentifier

parseConst :: Parser Syn.Expr
parseConst = debugParse "const" $ do
  c <- titularIdentifier
  t <- parseOptionalTypeAssignment
  pure $ Syn.Const c t

parseEBinder :: Parser Syn.Expr
parseEBinder = debugParse "ebinder" $ do
  reservedOp "\\"
  Syn.EBinder <$> parseBinder

parseBinder :: Parser Syn.Binder
parseBinder = debugParse "binder" $ do
  n <- identifier
  t <- parseOptionalTypeAssignment
  modifyState (Map.insert n t)
  pure $ Syn.Binder n t

parseLambda :: Parser Syn.Expr
parseLambda = debugParse "lambda" $ do
  reservedOp "\\"
  b <- parseBinder
  reservedOp "."
  Syn.Lam b <$> parseExpr'

parseApp :: Parser Syn.Expr
parseApp = do
  es <- many1 parseTerm
  pure (foldl1 Syn.App es)

parseUnivQ :: Parser Syn.Expr
parseUnivQ = debugParse "univq" $ do
  reservedOp "forall"
  b <- parseBinder
  reservedOp "."
  Syn.EQuant Syn.Univ b <$> parseExpr'

parseExisQ :: Parser Syn.Expr
parseExisQ = debugParse "exisq" $ do
  reservedOp "exists"
  b <- parseBinder
  reservedOp "."
  Syn.EQuant Syn.Exis b <$> parseExpr'

parsePred :: Parser Syn.Expr
parsePred = debugParse "pred" $ do
  n <- titularIdentifier
  t <- parseOptionalTypeAssignment
  args <- parens ((spaces *> parseExpr' <* spaces) `sepBy` char ',')
  pure $ Syn.Pred n t args

{-
parseSet :: Parser Syn.Expr
parseSet = brackets ((spaces *> parseExpr' <* spaces) `sepBy` char ',') <&> Syn.mkSet
-}

parseTypedef :: Parser Syn.Decl
parseTypedef = debugParse "typedef" $ do
  n <- titularIdentifier
  t <- parseTypeAssignment
  modifyState (Map.insert n t)
  pure $ Syn.Typedef n t

parseLet :: Parser Syn.Decl
parseLet = debugParse "let" $ do
  string "["
  name <- identifier
  string "]"
  spaces
  reservedOp "="
  spaces
  Syn.Let name <$> parseExpr'

parseDecl' = parseTypedef <|> parseLet

binOp :: String -> Syn.BinOp -> Ex.Assoc -> Ex.Operator String SymTypeState Identity Syn.Expr
binOp name fun = Ex.Infix (reservedOp name >> (pure $ \e0 -> \e1 -> Syn.EBinOp fun e0 e1))

comp :: String -> Syn.Comparison -> Ex.Assoc -> Ex.Operator String SymTypeState Identity Syn.Expr
comp name fun = Ex.Infix (reservedOp name >> (pure $ \e0 -> \e1 -> Syn.EComparison fun e0 e1))

unOp :: String -> Syn.UnOp -> Ex.Operator String SymTypeState Identity Syn.Expr
unOp name fun = Ex.Prefix (reservedOp name >> pure (Syn.EUnOp fun))

opTable :: Ex.OperatorTable String SymTypeState Identity Syn.Expr
opTable =
  [ [ binOp "*" Syn.Mul Ex.AssocLeft,
      binOp "/" Syn.Div Ex.AssocLeft
    ],
    [ binOp "+" Syn.Add Ex.AssocLeft,
      binOp "-" Syn.Sub Ex.AssocLeft
    ],
    [ comp "==" Syn.Eq Ex.AssocNone,
      comp "<" Syn.LT Ex.AssocNone,
      comp ">" Syn.GT Ex.AssocNone,
      comp "subs" Syn.SetSubS Ex.AssocNone,
      comp "elem" Syn.SetMem Ex.AssocRight
    ],
    [unOp "~" Syn.Neg],
    [ binOp "&" Syn.Conj Ex.AssocLeft,
      binOp "|" Syn.Disj Ex.AssocLeft
    ],
    [binOp "=>" Syn.Impl Ex.AssocRight],
    [unOp "compl" Syn.SetCompl],
    [ binOp "union" Syn.SetUnion Ex.AssocRight,
      binOp "inter" Syn.SetInter Ex.AssocRight,
      binOp "diff" Syn.SetDiff Ex.AssocRight
    ]
  ]

parseTerm :: Parser Syn.Expr
parseTerm =
  Ex.buildExpressionParser
    opTable
    ( parens parseExpr'
        <|> parseBool
        <|> parseNumber
        <|> try parseConst
        <|> try parseUnivQ
        <|> try parseExisQ
        <|> parseVar
        <|> try parseLambda
        <|> parseEBinder
    )

parseExpr' :: Parser Syn.Expr -- fixme
parseExpr' = debugParse "app" parseApp -- >>= pure . Syn.resolvePredicates

parseFrag' :: Parser [Syn.Decl]
parseFrag' = parseDecl' `sepBy` (spaces >> char ';' >> spaces)

-------------------------------------------------------------------------------
-- Entrypoints
-------------------------------------------------------------------------------

type ExprParse = Either ParseError Syn.Expr

type FragParse = Either ParseError [Syn.Decl]

parseFromFile :: Parser a -> FilePath -> IO (Either ParseError a)
parseFromFile p fname =
  do
    input <- readFile fname
    return (runP p Map.empty fname input)

parseDecl :: String -> Either ParseError Syn.Decl
parseDecl = runP (contents parseDecl') Map.empty "<stdin>"

parseExpr :: String -> ExprParse
parseExpr = runP (contents parseExpr') Map.empty "<stdin>"

parseFrag :: FilePath -> IO FragParse
parseFrag = parseFromFile (contents parseFrag')

parseFragS :: String -> FragParse
parseFragS = runP (contents parseFrag') Map.empty "<stdin>"

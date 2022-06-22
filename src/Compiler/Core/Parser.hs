module Compiler.Core.Parser (
  parseExpr,
  parseFrag,
  parseFragS,
  parseDecl,
  ParseError
) where

import System.IO (IO, FilePath)
import qualified Data.Map as Map
import Debug.Trace (trace, traceM)
import Data.Functor.Identity
import Control.Monad (void)
import Text.Parsec
import qualified Text.Parsec.Expr as Ex
import Text.ParserCombinators.Parsec.Combinator (choice)

import Compiler.Core.Lexer
import qualified Compiler.Core.Syntax as Syn
import Compiler.Core.Pretty

debugParse :: String -> Parser a -> Parser a
debugParse s p = if False
  then (tryParse s) *> p <* (completeParse s)
  else p
    where
      tryParse p = parserTrace ("parsing " ++ p ++ "...")
      completeParse p = parserTrace ("ok, parsed " ++ p)

titularIdentifier = (lookAhead upper) >> identifier
lIdentifier = (lookAhead lower) >> identifier

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

tyatom :: Parser Syn.Type
tyatom = tylit <|> (angles parseType)

tylit :: Parser Syn.Type
tylit = (reservedOp "t" >> pure Syn.tyBool)
     <|> (reservedOp "n" >> pure Syn.tyInt)
     <|> (titularIdentifier >>= (pure . Syn.TyVar . Syn.TV))
     <|> (identifier >>= (pure . Syn.TyCon))

parseType :: Parser Syn.Type
parseType = Ex.buildExpressionParser tyops tyatom
  where
    infixOp x f = Ex.Infix (reservedOp x >> pure f)
    tyops = [ [infixOp "," Syn.TyFun Ex.AssocRight]
            ]

-------------------------------------------------------------------------------
-- Expressions
-------------------------------------------------------------------------------

parseTypeAssignment :: Parser Syn.Type
parseTypeAssignment = reservedOp ":" >> parseType

parseBool :: Parser Syn.Expr
parseBool = debugParse "bool" (reserved "True" >> pure (Syn.ELit (Syn.LBool True)))
         <|> (reserved "False" >> pure (Syn.ELit (Syn.LBool False)))

parseNumber :: Parser Syn.Expr
parseNumber = debugParse "number" $ do
  n <- natural
  pure (Syn.ELit (Syn.LInt (fromIntegral n)))

parseVar :: Parser Syn.Expr
parseVar = debugParse "var" $ do
  i <- lIdentifier
  pure $ Syn.Var i

parseConst :: Parser Syn.Expr
parseConst = debugParse "const" $ do
  c <- titularIdentifier
  s <- getState
  t <- parseTypeAssignment <|> (pure $ s Map.! c)
  pure $ Syn.Const c t

parseEBinder :: Parser Syn.Expr
parseEBinder = debugParse "ebinder" $ do
  reservedOp "\\"
  b <- parseBinder
  pure $ Syn.EBinder b

parseBinder :: Parser Syn.Binder
parseBinder = debugParse "binder" $ do
  n <- identifier
  t <- parseTypeAssignment
  modifyState (Map.insert n t)
  pure $ Syn.Binder n t

parseLambda :: Parser Syn.Expr
parseLambda = debugParse "lambda" $ do
  reservedOp "\\"
  b <- parseBinder
  reservedOp "."
  e <- parseExpr'
  pure (Syn.Lam b e)

parseApp :: Parser Syn.Expr
parseApp = do
  es <- many1 parseTerm
  pure (foldl1 Syn.App es)

parseUnivQ :: Parser Syn.Expr
parseUnivQ = debugParse "univq" $ do
  reservedOp "forall"
  b <- parseBinder
  reservedOp "."
  e <- parseExpr'
  pure $ Syn.EQuant Syn.Univ b e

parseExisQ :: Parser Syn.Expr
parseExisQ = debugParse "exisq" $ do
  reservedOp "exists"
  b <- parseBinder
  reservedOp "."
  e <- parseExpr'
  pure $ Syn.EQuant Syn.Exis b e

parsePred :: Parser Syn.Expr
parsePred = debugParse "pred" $ do
  n <- titularIdentifier
  t <- parseTypeAssignment
  args <- parens ((spaces *> parseExpr' <* spaces) `sepBy` char ',')
  pure $ Syn.Pred n t args

parseSet :: Parser Syn.Expr
parseSet = brackets ((spaces *> parseExpr' <* spaces) `sepBy` char ',') >>= (pure . Syn.mkSet)

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
  expr <- parseExpr'
  pure $ Syn.Let name expr

parseDecl' = parseTypedef <|> parseLet

factor :: Parser Syn.Expr
factor = (parens parseExpr') <|>
         (parseBool)         <|>
         (parseNumber)       <|>
         (try parseConst)    <|>
         (try parseUnivQ)        <|>
         (try parseExisQ)        <|>
         (parseVar)          <|>
         (try parseLambda)       <|>
         parseEBinder
         -- (parsePred)

binOp :: String -> Syn.BinOp -> Ex.Assoc -> Ex.Operator String SymTypeState Identity Syn.Expr
binOp name fun assoc = Ex.Infix (reservedOp name >> (pure $ \e0 -> \e1 -> Syn.EBinOp fun e0 e1)) assoc

comp :: String -> Syn.Comparison -> Ex.Assoc -> Ex.Operator String SymTypeState Identity Syn.Expr
comp name fun assoc = Ex.Infix (reservedOp name >> (pure $ \e0 -> \e1 -> Syn.EComparison fun e0 e1)) assoc

unOp :: String -> Syn.UnOp -> Ex.Operator String SymTypeState Identity Syn.Expr
unOp name fun = Ex.Prefix (reservedOp name >> (pure $ \e -> Syn.EUnOp fun e))

opTable :: Ex.OperatorTable String SymTypeState Identity Syn.Expr
opTable = [ [ binOp "*" Syn.Mul Ex.AssocLeft
            , binOp "/" Syn.Div Ex.AssocLeft ]
          , [ binOp "+" Syn.Add Ex.AssocLeft
            , binOp "-" Syn.Sub Ex.AssocLeft ]
          , [ comp "==" Syn.Eq Ex.AssocNone
            , comp "<" Syn.LT Ex.AssocNone
            , comp ">" Syn.GT Ex.AssocNone 
            , comp "subs" Syn.SetSubS Ex.AssocNone
            , comp "elem" Syn.SetMem Ex.AssocRight ]
          , [unOp "~" Syn.Neg]
          , [ binOp "&" Syn.Conj Ex.AssocLeft
            , binOp "|" Syn.Disj Ex.AssocLeft ]
          , [binOp "=>" Syn.Impl Ex.AssocRight]
          , [unOp "compl" Syn.SetCompl]
          , [ binOp "union" Syn.SetUnion Ex.AssocRight
            , binOp "inter" Syn.SetInter Ex.AssocRight
            , binOp "diff" Syn.SetDiff Ex.AssocRight ]
          ]

parseTerm :: Parser Syn.Expr
parseTerm = Ex.buildExpressionParser opTable factor

parseExpr' :: Parser Syn.Expr          -- fixme
parseExpr' = debugParse "app" parseApp -- >>= pure . Syn.resolvePredicates

parseFrag' :: Parser [Syn.Decl]
parseFrag' = parseDecl' `sepBy` (spaces >> char ';' >> spaces)

-------------------------------------------------------------------------------
-- Entrypoints
-------------------------------------------------------------------------------

type ExprParse = Either ParseError Syn.Expr
type FragParse = Either ParseError [Syn.Decl]

parseFromFile :: Parser a -> FilePath -> IO (Either ParseError a)
parseFromFile p fname
    = do input <- readFile fname
         return (runP p Map.empty fname input)

parseDecl :: String -> Either ParseError Syn.Decl
parseDecl input = runP (contents parseDecl') Map.empty "<stdin>" input

parseExpr :: String -> ExprParse
parseExpr input = runP (contents parseExpr') Map.empty "<stdin>" input

parseFrag :: FilePath -> IO FragParse
parseFrag fp = parseFromFile (contents parseFrag') fp

parseFragS :: String -> FragParse
parseFragS input = runP (contents parseFrag') Map.empty "<stdin>" input

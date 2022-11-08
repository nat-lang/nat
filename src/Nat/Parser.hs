{-# LANGUAGE OverloadedStrings #-}

module Nat.Parser
  ( Lex.IndentOpt (..),
    Lex.indentBlock,
    Operator (..),
    makeExprParser,
    C.char,
    C.space1,
    dbg,
    module Nat.Parser,
    module Text.Megaparsec,
  )
where

import Control.Monad.Combinators.Expr
  ( Operator (..),
    makeExprParser,
  )
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.State
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Void (Void)
import Debug.Trace (traceM)
import Text.Megaparsec
  ( ParseErrorBundle,
    ParsecT,
    between,
    choice,
    lookAhead,
    many,
    notFollowedBy,
    observing,
    parseError,
    runParserT,
    sepBy,
    sepEndBy,
    some,
    try,
    (<|>),
  )
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Char.Lexer as Lex
import Text.Megaparsec.Debug (dbg)

newtype ParseState = ParseState {inTree :: Bool}

type Parser = ParsecT Void Text (StateT ParseState Identity)

-- type NatParseErrorBundle = ParseErrorBundle

pState = ParseState {inTree = False}

keywords :: [String]
keywords =
  [ "if",
    "then",
    "else",
    "let",
    "letrec",
    "dom",
    "case",
    "tycase",
    "of",
    ":",
    "undef",
    "forall",
    "exists",
    "the",
    "in"
  ]

lineComment = L.skipLineComment "//"

blockComment = L.skipBlockComment "/*" "*/"

space :: Parser ()
space = L.space C.hspace1 lineComment blockComment

spaceN :: Parser ()
spaceN = L.space C.space1 lineComment blockComment

delimiter = some (space >> some C.newline >> space)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: Text -> Parser Text
symbol = L.symbol space

btw sym0 sym1 = between (symbol sym0) (symbol sym1)

brackets :: Parser a -> Parser a
brackets = btw "[" "]"

parens :: Parser a -> Parser a
parens = btw "(" ")"

angles :: Parser a -> Parser a
angles = btw "<" ">"

curlies :: Parser a -> Parser a
curlies = btw "{" "}"

comma = space >> C.char ',' >> space

commaSep :: Parser a -> Parser [a]
commaSep p = p `sepBy` comma

pipeSep :: Parser a -> Parser [a]
pipeSep p = p `sepBy` try (C.space >> C.char '|' >> C.space)

reserved :: Text -> Parser ()
reserved w = (lexeme . try) (C.string w *> notFollowedBy C.alphaNumChar)

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> C.letterChar <*> many (C.alphaNumChar <|> C.char '\'')
    check x =
      if x `elem` keywords
        then fail $ "keyword " ++ show x ++ " cannot be an identifier"
        else pure x

titularIdentifier = lookAhead C.upperChar >> identifier

lIdentifier = lookAhead C.lowerChar >> identifier

integer :: Parser Integer
integer = lexeme L.decimal

infixOpL, infixOpR :: Text -> (a -> a -> a) -> Operator Parser a
infixOpL name f = InfixL (f <$ symbol name)
infixOpR name f = InfixR (f <$ symbol name)

prefixOp, postfixOp :: Text -> (a -> a) -> Operator Parser a
prefixOp name f = Prefix (f <$ symbol name)
postfixOp name f = Postfix (f <$ symbol name)

parse ::
  ParsecT e s (StateT ParseState Identity) a ->
  s ->
  Either (ParseErrorBundle s e) a
parse parser i = runIdentity $ evalStateT (runParserT parser "<input>" i) pState

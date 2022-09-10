{-# LANGUAGE OverloadedStrings #-}

module Mean.Parser
  ( 
    Lex.IndentOpt(..), Lex.indentBlock,
    Operator(..), makeExprParser,
    C.char,
    module Mean.Parser,
    module Text.Megaparsec
  )
where

import Control.Monad.Combinators.Expr
    ( Operator(..), makeExprParser )
import Debug.Trace (traceM)
import Text.Megaparsec ((<|>), try, some, many, sepBy, choice, between, notFollowedBy, lookAhead, runParserT, ParseErrorBundle, ParsecT)
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as Lex
import Text.Megaparsec.Debug (dbg)

import Control.Monad.Identity ( runIdentity, Identity )
import Data.Functor ((<&>))
import Data.Text (Text)
import qualified Data.Text.IO as TiO
import Control.Monad.State
import Data.Void (Void)
import qualified Text.Megaparsec.Char.Lexer as L

newtype ParseState = ParseState {inTree :: Bool}

type Parser = ParsecT Void Text (StateT ParseState Identity)

keywords :: [String]
keywords = ["if", "then", "else", "let", "dom", "case", "of"]

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

indent :: (Show a) => ([b] -> Parser a) -> Parser b -> Parser a
indent p0 p1 = dbg "indent" $ L.indentBlock spaceN p
  where
    p = pure $ L.IndentSome Nothing p0 p1

parse parser i = runIdentity $ evalStateT (runParserT parser "<input>" i) (ParseState {inTree = False})

parseFile parser file = TiO.readFile file <&> \i -> evalStateT (runParserT parser file i) (ParseState {inTree = False})

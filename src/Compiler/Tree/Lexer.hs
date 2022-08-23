module Compiler.Tree.Lexer where

import Compiler.Tree.Syntax
import Control.Monad.State
import Data.Void (Void)
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as Parsec
import qualified Text.Megaparsec.Char as CharParser
import qualified Text.Megaparsec.Char.Lexer as Lexer

type Parser = Parsec.Parsec Void String

space :: Parser ()
space = Lexer.space CharParser.space1 lineComment blockComment
  where
    lineComment = Lexer.skipLineComment "//"
    blockComment = Lexer.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme space

symbol :: String -> Parser String
symbol = Lexer.symbol space

brackets :: Parser a -> Parser a
brackets = Parsec.between (symbol "[") (symbol "]")

parens :: Parser a -> Parser a
parens = Parsec.between (symbol "(") (symbol ")")

identifier :: Parser String
identifier = lexeme $ Parsec.try p
  where
    p = (:) <$> CharParser.letterChar <*> Parsec.many (CharParser.alphaNumChar <|> CharParser.char '\'')

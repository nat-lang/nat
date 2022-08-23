{-# LANGUAGE OverloadedStrings #-}

module Mean.Lexer where

import Compiler.Tree.Syntax
import Control.Monad.State
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = P.Parsec Void Text

space :: Parser ()
space = L.space C.space1 lineComment blockComment
  where
    lineComment = L.skipLineComment "//"
    blockComment = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: Text -> Parser Text
symbol = L.symbol space

brackets :: Parser a -> Parser a
brackets = P.between (symbol "[") (symbol "]")

parens :: Parser a -> Parser a
parens = P.between (symbol "(") (symbol ")")

identifier :: Parser String
identifier = lexeme $ P.try p
  where
    p = (:) <$> C.letterChar <*> P.many (C.alphaNumChar <|> C.char '\'')

titularIdentifier = P.lookAhead C.upperChar >> identifier

lIdentifier = P.lookAhead C.lowerChar >> identifier

reserved :: Text -> Parser ()
reserved w = (lexeme . P.try) (C.string w *> P.notFollowedBy C.alphaNumChar)

integer :: Parser Integer
integer = lexeme L.decimal
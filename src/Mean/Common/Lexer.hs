{-# LANGUAGE OverloadedStrings #-}

module Mean.Common.Lexer where

import Control.Monad.Combinators.Expr (Operator (..))
import Control.Monad.Identity (Identity)
import Control.Monad.State
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

newtype ParseState = ParseState {inTree :: Bool}

type Parser = P.ParsecT Void Text (StateT ParseState Identity)

space :: Parser ()
space = L.space C.hspace1 lineComment blockComment
  where
    lineComment = L.skipLineComment "//"
    blockComment = L.skipBlockComment "/*" "*/"

delimiter = P.some (space >> P.some C.newline >> space)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: Text -> Parser Text
symbol = L.symbol space

btw sym0 sym1 = P.between (symbol sym0) (symbol sym1)

brackets :: Parser a -> Parser a
brackets = btw "[" "]"

parens :: Parser a -> Parser a
parens = btw "(" ")"

angles :: Parser a -> Parser a
angles = btw "<" ">"

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

infixOpL, infixOpR :: Text -> (a -> a -> a) -> Operator Parser a
infixOpL name f = InfixL (f <$ symbol name)
infixOpR name f = InfixR (f <$ symbol name)

prefixOp, postfixOp :: Text -> (a -> a) -> Operator Parser a
prefixOp name f = Prefix (f <$ symbol name)
postfixOp name f = Postfix (f <$ symbol name)
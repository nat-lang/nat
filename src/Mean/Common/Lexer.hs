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
import Text.Megaparsec.Debug (dbg)

newtype ParseState = ParseState {inTree :: Bool}

type Parser = P.ParsecT Void Text (StateT ParseState Identity)

keywords :: [String]
keywords = ["if", "then", "else", "let", "dom", "case", "of"]

lineComment = L.skipLineComment "//"

blockComment = L.skipBlockComment "/*" "*/"

space :: Parser ()
space = L.space C.hspace1 lineComment blockComment

spaceN :: Parser ()
spaceN = L.space C.space1 lineComment blockComment

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

curlies :: Parser a -> Parser a
curlies = btw "{" "}"

reserved :: Text -> Parser ()
reserved w = (lexeme . P.try) (C.string w *> P.notFollowedBy C.alphaNumChar)

identifier :: Parser String
identifier = (lexeme . P.try) (p >>= check)
  where
    p = (:) <$> C.letterChar <*> P.many (C.alphaNumChar <|> C.char '\'')
    check x =
      if x `elem` keywords
        then fail $ "keyword " ++ show x ++ " cannot be an identifier"
        else pure x

titularIdentifier = P.lookAhead C.upperChar >> identifier

lIdentifier = P.lookAhead C.lowerChar >> identifier

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

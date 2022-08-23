module Compiler.Core.Lexer where

import qualified Compiler.Core.Syntax as Syn
import qualified Data.Map as Map
import Text.Parsec (Parsec, alphaNum, eof, letter, oneOf, (<|>))
import Text.Parsec.Language (haskellStyle)
import qualified Text.Parsec.Token as Tok
import qualified Text.ParserCombinators.Parsec.Char as CTok

-------------------------------------------------------------------------------
-- State def
-------------------------------------------------------------------------------

type SymTypeState = Map.Map Syn.Name Syn.Type

type Parser = Parsec String SymTypeState

-------------------------------------------------------------------------------
-- Lex
-------------------------------------------------------------------------------

reservedNames :: [String]
reservedNames =
  [ "exists",
    "forall"
  ]

reservedOps :: [String]
reservedOps =
  [ "->",
    "\\",
    "+",
    "*",
    "-",
    "=",
    ":"
  ]

lexer :: Tok.TokenParser SymTypeState
lexer =
  Tok.makeTokenParser $
    Tok.LanguageDef
      { Tok.commentStart = "{-",
        Tok.commentEnd = "-}",
        Tok.commentLine = "--",
        Tok.nestedComments = True,
        Tok.identStart = letter,
        Tok.identLetter = alphaNum <|> oneOf "_'",
        Tok.opStart = oneOf ":!#$%&*+./<=>?@\\^|-~",
        Tok.opLetter = oneOf ":!#$%&*+./=?@\\^|-~",
        Tok.reservedNames = [], -- reservedNames
        Tok.reservedOpNames = [], -- reservedOps
        Tok.caseSensitive = True
      }

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

identifier :: Parser String
identifier = Tok.identifier lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

angles :: Parser a -> Parser a
angles = Tok.angles lexer

brackets :: Parser a -> Parser a
brackets = Tok.brackets lexer

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

natural :: Parser Integer
natural = Tok.natural lexer
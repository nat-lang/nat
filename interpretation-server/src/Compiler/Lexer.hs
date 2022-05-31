module Compiler.Lexer where

import qualified Data.Map as Map
import Text.Parsec
import qualified Text.Parsec.Token as Tok
import qualified Text.ParserCombinators.Parsec.Char as CTok
import Text.Parsec.Language (haskellStyle)

import qualified Compiler.Syntax as Syn

-------------------------------------------------------------------------------
-- State
-------------------------------------------------------------------------------

type SymTypeState = Map.Map String Syn.Type
type Parser = Parsec String SymTypeState

reservedNames :: [String]
reservedNames = [
    "exists",
    "forall"
  ]

reservedOps :: [String]
reservedOps = [
    "->",
    "\\",
    "+",
    "*",
    "-",
    "=",
    ":"
  ]

lexer :: Tok.TokenParser SymTypeState
lexer = Tok.makeTokenParser $ Tok.LanguageDef
  { Tok.commentStart    = "{-"
  , Tok.commentEnd      = "-}"
  , Tok.commentLine     = "--"
  , Tok.nestedComments  = True
  , Tok.identStart      = letter
  , Tok.identLetter     = alphaNum <|> oneOf "_'"
  , Tok.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.opLetter        = oneOf ":!#$%&*+./=?@\\^|-~"
  , Tok.reservedNames   = [] -- reservedNames
  , Tok.reservedOpNames = [] -- reservedOps
  , Tok.caseSensitive   = True
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
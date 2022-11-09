module Nat.Syntax.Token where

import Nat.Syntax.Literal
import Nat.Syntax.Position

type FileInterval = Interval FilePath

data Keyword
  = KwIf
  | KwThen
  | KwElse
  | KwLet
  | KwDom
  | KwCase
  | KwTycase
  | KwOf
  | KwUndef
  | KwForall
  | KwExists
  | KwThe
  | KwIn
  | KwDo

data Symbol
  = SymBackslash
  | SymDot
  | SymColon
  | SymArrow
  | SymPipe
  | SymLodash
  | SymEllipse
  | SymOpenParen
  | SymCloseParen
  | SymOpenBrace
  | SymCloseBrace
  | SymOpenChevr
  | SymCloseChevr

data Token
  = TokKeyword Keyword FileInterval
  | TokLiteral Literal FileInterval
  | TokIdent String FileInterval
  | TokSym Symbol FileInterval
  | TokComment String FileInterval
  | TokTeX String FileInterval
  | TokEOF FileInterval
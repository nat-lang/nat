{-# LANGUAGE OverloadedStrings #-}

module Mean.Module.Parser where

import Data.Void
import Data.Text
import qualified Text.Megaparsec as P
import qualified Mean.Common.Lexer as L
import Mean.Common.Parser (parseFile)
import qualified Mean.Core.Parser as Core
import qualified Mean.Sugar.Parser as Sugar
import Mean.Module.Syntax

type ExprParser = L.Parser Expr
type ModuleParse = Either (P.ParseErrorBundle Text Data.Void.Void) Module

pDecl :: ExprParser
pDecl = do
  L.reserved "let"
  name <- L.identifier
  L.space
  L.symbol "="
  L.space
  Decl name <$> Sugar.pExpr

pModule :: L.Parser Module
pModule = pDecl `P.sepBy` L.delimiter

pFModule :: String -> IO ModuleParse
pFModule = parseFile pModule

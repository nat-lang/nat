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

type ExprParser = L.Parser ModuleExpr
type ModuleParse = Either (P.ParseErrorBundle Text Data.Void.Void) Module

pMDecl :: ExprParser
pMDecl = do
  L.reserved "let"
  name <- L.identifier
  L.symbol "="
  MDecl name <$> Sugar.pSExpr

pModule :: L.Parser Module
pModule = pMDecl `P.sepBy` L.delimiter

pFModule :: String -> IO ModuleParse
pFModule = parseFile pModule

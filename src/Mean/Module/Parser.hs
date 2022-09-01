{-# LANGUAGE OverloadedStrings #-}

module Mean.Module.Parser where

import Data.Text
import Data.Void
import qualified Mean.Common.Lexer as L
import Mean.Common.Parser (parseFile)
import qualified Mean.Core.Parser as Core
import Mean.Module.Syntax
import qualified Mean.Sugar.Parser as Sugar
import qualified Text.Megaparsec as P

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

-- pFModule :: String -> IO ModuleParse
pFModule = parseFile pModule

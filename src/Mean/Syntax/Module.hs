{-# LANGUAGE OverloadedStrings #-}

module Mean.Syntax.Module where

import Data.Text
import Data.Void
import Debug.Trace (traceM)
import Mean.Syntax.Surface
import qualified Mean.Parser as P

data ModuleExpr = MDecl Name Expr deriving (Eq, Show)

type Module = [ModuleExpr]

-------------------------------------------------------------------------------
-- Parsing
-------------------------------------------------------------------------------

type MExprParser = P.Parser ModuleExpr

type ModuleParse = Either (P.ParseErrorBundle Text Data.Void.Void) Module

pMDecl :: MExprParser
pMDecl = do
  P.reserved "let"
  name <- P.identifier
  P.symbol "="
  MDecl name <$> pExpr

pModule :: P.Parser Module
pModule = pMDecl `P.sepBy` P.delimiter

pFModule = P.parseFile pModule

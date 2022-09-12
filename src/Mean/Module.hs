{-# LANGUAGE OverloadedStrings #-}

module Mean.Module where

import Debug.Trace (traceM)
import Data.Text
import Data.Void
import Mean.Core hiding (Eq)
import Mean.Syntax
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

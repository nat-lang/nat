{-# LANGUAGE OverloadedStrings #-}

module Mean.Syntax.Module where

import Data.Text
import Data.Void
import Debug.Trace (traceM)
import qualified Mean.Parser as P
import Mean.Syntax.Surface
import Mean.Var
import Text.Megaparsec.Debug (dbg)

data ModuleExpr
  = MDecl Var Expr
  | MExec Expr
  | MOut [Expr]
  deriving (Eq, Show, Ord)

type Module = [ModuleExpr]

-------------------------------------------------------------------------------
-- Parsing
-------------------------------------------------------------------------------

pMDecl :: P.Parser ModuleExpr
pMDecl = do
  P.reserved "let"
  v <- pVar
  P.symbol "="
  MDecl v <$> pExpr

pMExec :: P.Parser ModuleExpr
pMExec = MExec <$> pExpr

pMExpr = P.choice [pMExec, pMDecl]

pModule :: P.Parser Module
pModule = dbg "mod" $ pMExpr `P.sepEndBy` P.delimiter

pFModule = P.parseFile pModule

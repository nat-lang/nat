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

pMLetRecDecl :: P.Parser ModuleExpr
pMLetRecDecl = do
  P.reserved "letrec"
  v <- pVar
  P.symbol "="
  e <- pExpr
  pure $ MDecl v (EFix v e)

pMLetDecl :: P.Parser ModuleExpr
pMLetDecl = do
  P.reserved "let"
  v <- pVar
  P.symbol "="
  MDecl v <$> pExpr

pMDecl :: P.Parser ModuleExpr
pMDecl = P.choice [pMLetRecDecl, pMLetDecl]

pMExec :: P.Parser ModuleExpr
pMExec = MExec <$> pExpr

pMExpr = P.choice [pMExec, pMDecl]

pModule :: P.Parser Module
pModule = pMExpr `P.sepEndBy` P.delimiter

pFModule = P.parseFile pModule

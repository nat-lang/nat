{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Mean.Syntax.Module where

import Data.Text
import Data.Void
import Debug.Trace (traceM)
import Mean.Context
import qualified Mean.Parser as P
import Mean.Syntax.Surface
import Mean.Walk
import Text.Megaparsec.Debug (dbg)

data ModuleExpr
  = MDecl Var Expr
  | MLetRec Var Expr
  | MExec Expr
  deriving (Eq, Show, Ord)

type Module = [ModuleExpr]

-------------------------------------------------------------------------------
-- Parsing
-------------------------------------------------------------------------------

pMLetRec :: P.Parser ModuleExpr
pMLetRec = do
  P.reserved "letrec"
  v <- pVar
  P.symbol "="
  MLetRec v <$> pExpr

pMDecl :: P.Parser ModuleExpr
pMDecl = do
  P.reserved "let"
  v <- pVar
  P.symbol "="
  MDecl v <$> pExpr

pMExec :: P.Parser ModuleExpr
pMExec = MExec <$> pExpr

pMExpr = P.choice [pMExec, pMLetRec, pMDecl]

pModule :: P.Parser Module
pModule = pMExpr `P.sepEndBy` P.delimiter

pFModule = P.parseFile pModule

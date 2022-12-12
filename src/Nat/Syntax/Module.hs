{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Nat.Syntax.Module where

import Data.Text
import Data.Void
import Debug.Trace (traceM)
import Nat.Context
import qualified Nat.Parser as P
import Nat.Syntax.Surface
import Nat.Syntax.Type
import Nat.Walk
import Text.Megaparsec.Debug (dbg)

type ModulePath = [String]

data ModuleExprR e
  = MDecl Var e
  | MImport [Var] ModulePath
  | MExec e
  deriving (Eq, Show, Ord, Functor, Foldable, Traversable)

type ModuleExpr = ModuleExprR Expr

type Module = [ModuleExpr]

data NamedModule = NMod ModulePath Module

-------------------------------------------------------------------------------
-- Parsing
-------------------------------------------------------------------------------

pMDecl :: P.Parser ModuleExpr
pMDecl = do
  P.reserved "let"
  v <- pVar
  P.symbol "="
  MDecl v <$> pExpr

pMImport :: P.Parser ModuleExpr
pMImport = do
  P.reserved "import"
  vs <- P.parens (P.commaSep pVar)
  P.symbol "from"
  MImport vs <$> P.dotSep P.identifier

pMExec :: P.Parser ModuleExpr
pMExec = MExec <$> pExpr

pEDom v = EDom . Dom (TyCon v) <$> pSet

pMDom = do
  P.reserved "dom"
  v <- pVar
  P.symbol "="
  MDecl v <$> pEDom v

pMExpr = P.choice [pMExec, pMDecl, pMDom, pMImport]

pModule :: P.Parser Module
pModule = pMExpr `P.sepEndBy` P.delimiter

runPModule = P.parse pModule
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

data ModuleExprR e
  = MDecl Var e
  | MExec e
  deriving (Eq, Show, Ord, Functor, Foldable, Traversable)

type ModuleExpr = ModuleExprR Expr

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

pEDom v = EDom . Dom (TyCon v) <$> pSet

pMDom = do
  P.reserved "dom"
  v <- pVar
  P.symbol "="
  MDecl v <$> pEDom v

pMExpr = P.choice [pMExec, pMDecl, pMDom]

pModule :: P.Parser Module
pModule = pMExpr `P.sepEndBy` P.delimiter

runPModule = P.parse pModule
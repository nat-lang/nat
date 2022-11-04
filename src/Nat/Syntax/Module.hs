{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
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
  | MLetRec Var e
  | MStruct Var e [e]
  | MImport Module
  | MExec e
  deriving (Eq, Show, Ord, Functor, Foldable, Traversable)

type ModuleExpr = ModuleExprR Expr

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

pEDom v = EDom . Dom (TyCon v) <$> pSet

pMDom = do
  P.reserved "dom"
  v <- pVar
  P.symbol "="
  MDecl v <$> pEDom v

pMExpr = P.choice [pMExec, pMLetRec, pMDecl, pMDom]

pModule :: P.Parser Module
pModule = pMExpr `P.sepEndBy` P.delimiter

pFModule = P.parseFile pModule

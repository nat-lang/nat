{-# LANGUAGE OverloadedStrings #-}

module Mean.Core.Parser where

import Control.Applicative (some)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Functor ((<&>))
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Mean.Common.Lexer as L
import Mean.Common.Parser (parseFile)
import qualified Mean.Core.Syntax as S
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as C
import Text.Megaparsec.Debug (dbg)
import Mean.Core.Viz
-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

type TyParser = L.Parser S.Type

pTyTerm :: TyParser
pTyTerm =
  P.choice
    [ L.angles pType,
      L.reserved "t" >> pure S.tyBool,
      L.reserved "n" >> pure S.tyInt,
      L.titularIdentifier <&> (S.TyVar . S.TV),
      L.identifier <&> S.TyCon
    ]

tyNil :: TyParser
tyNil = pure S.TyNil

pType :: TyParser
pType = makeExprParser pTyTerm tyOps
  where
    tyOps =
      [ [L.infixOpR "," S.TyFun]
      ]

pTypeAssignment :: TyParser
pTypeAssignment = (L.reserved ":" >> pType) <|> tyNil

pOptionalTypeAssignment :: TyParser
pOptionalTypeAssignment = pTypeAssignment <|> tyNil

-------------------------------------------------------------------------------
-- Terms
-------------------------------------------------------------------------------

type ExprParser = L.Parser S.CoreExpr

type LitParser = L.Parser S.Lit

pBool :: LitParser
pBool =
  (L.reserved "True" >> pure (S.LBool True))
    <|> (L.reserved "False" >> pure (S.LBool False))

pInt :: LitParser
pInt = S.LInt . fromIntegral <$> L.integer

pLit :: LitParser
pLit = P.choice [pInt, pBool]

pCLit :: ExprParser
pCLit = S.CLit <$> pLit

pVar :: L.Parser S.Var
pVar = S.mkVar <$> L.identifier

pCVar :: ExprParser
pCVar = S.CVar <$> pVar

pBinder :: L.Parser S.Binder
pBinder = do
  L.symbol "\\"
  n <- L.identifier
  S.Binder (S.mkVar n) <$> pOptionalTypeAssignment

pLam :: L.Parser (a -> S.Lambda a)
pLam = do
  b <- pBinder
  L.symbol "."
  pure $ S.Lam b

pCLam :: ExprParser
pCLam = do
  lam <- pLam
  S.CLam . lam <$> pCExpr

pCond :: L.Parser a -> L.Parser (S.Conditional a)
pCond pExpr = do
  L.reserved "if"
  x <- pExpr
  L.reserved "then"
  y <- pExpr
  L.reserved "else"
  S.Cond x y <$> pExpr

pCCond = S.CCond <$> pCond pCExpr

pCTerm :: ExprParser
pCTerm =
  P.choice
    [ L.parens pCExpr,
      pCLit,
      pCVar,
      pCLam,
      pCCond
    ]

operatorTable :: [[Operator L.Parser S.CoreExpr]]
operatorTable =
  [ [],
    -- prefix "-" Negation,
    -- prefix "+" id

    [],
    -- infix "*" Product,
    -- infix "/" Division

    []
  ]

-- infix "+" Sum,
-- infix "-" Subtr

pCExpr' :: ExprParser
pCExpr' = makeExprParser pCTerm operatorTable

pCExpr :: ExprParser
pCExpr = do
  exprs <- some pCExpr'
  pure (foldl1 (\e0 e1 -> S.CApp $ S.App e0 e1) exprs)

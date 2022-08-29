{-# LANGUAGE OverloadedStrings #-}

module Mean.Core.Parser
  ( P.ParseError (..),
    pExpr,
    pVar,
    pInt,
    pBool,
    pLam,
    pType,
  )
where

import Control.Applicative (some)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Functor ((<&>))
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Mean.Common.Lexer as L
import qualified Mean.Core.Syntax as S
import Text.Megaparsec ((<|>))
import Mean.Common.Parser (parseFile)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as C

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

type ExprParser = L.Parser S.Expr

pBool :: ExprParser
pBool =
  (L.reserved "True" >> pure (S.ELit (S.LBool True)))
    <|> (L.reserved "False" >> pure (S.ELit (S.LBool False)))

pInt :: ExprParser
pInt = S.ELit . S.LInt . fromIntegral <$> L.integer

pVar :: ExprParser
pVar = do
  S.mkEVar <$> L.identifier

pBinder :: L.Parser S.Binder
pBinder = do
  n <- L.identifier
  S.Binder (S.mkVar n) <$> pOptionalTypeAssignment

pLam :: ExprParser
pLam = do
  L.symbol "\\"
  b <- pBinder
  L.symbol "."
  S.Lam b <$> pExpr

pTerm :: ExprParser
pTerm =
  P.choice
    [ L.parens pExpr,
      pInt,
      pBool,
      pVar,
      pLam
    ]

operatorTable :: [[Operator L.Parser S.Expr]]
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

pExpr' :: ExprParser
pExpr' = makeExprParser pTerm operatorTable

pExpr :: ExprParser
pExpr = do
  exprs <- some pExpr'
  pure (foldl1 S.App exprs)

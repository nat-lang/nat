{-# LANGUAGE GADTs, OverloadedStrings #-}

module Mean.Relations where

import Mean.Core
import Mean.Viz
import qualified Mean.Parser as P
import Text.PrettyPrint
    ( Doc, (<+>), char, text )
import Prelude hiding ((&&), (||))
import qualified Prelude as Prel
import Control.Monad.Except ( MonadError(throwError) )

data UnOp = Neg

data BinOp = NEq | And | Or

data TernOp = Cond

data RelExpr where
  RUnOp :: Expressible a => UnOp -> a -> RelExpr
  RBinOp :: Expressible a => BinOp -> a -> a -> RelExpr
  RTernOp :: Expressible a => TernOp -> a -> a -> a -> RelExpr

instance Pretty RelExpr where
  ppr p e = case e of
    RBinOp op e0 e1 -> ppr p e0 <+> text ppOp <+> ppr p e1
      where
        ppOp = case op of
          NEq -> "!="
          And -> "&"
          Or -> "|"
    RUnOp op e -> ppr p e <+> char ppOp
      where
        ppOp = case op of
          Neg -> '¬'
    RTernOp Cond x y z -> text "if" <+> ppr p x <+> text "then" <+> ppr p y <+> text "else" <+> ppr p z

instance Show RelExpr where
  show = show . ppr 0

bool :: CoreExpr -> Bool
bool e = case e of
  (CLit (LBool b)) -> b
  _ -> error "can only extract bool from literal bool"

instance Reducible RelExpr where
  reduce expr = case expr of
    RUnOp op e -> do
      e' <- reduce e
      pure $
        mkCBool $ case op of
          Neg -> not $ bool e'
    RBinOp op e0 e1 -> do
      e0' <- reduce e0
      e1' <- reduce e1
      pure $
        mkCBool $ case op of
          NEq -> e0' /= e1'
          And -> bool e0' Prel.&& bool e1'
          Or -> bool e0' Prel.|| bool e1'
    RTernOp Cond x y z -> do
      x' <- reduce x
      case x' of
        CLit LBool {} -> reduce $ if bool x' then y else z
        _ -> throwError (NotTruthy x')

(?) :: Expressible a => a -> a -> (a -> RelExpr)
(?) = RTernOp Cond

(>) :: (a -> RelExpr) -> a -> RelExpr
e > e' = e e'

(&&) :: Expressible a => a -> a -> RelExpr
p && q = RBinOp And p q

(||) :: Expressible a => a -> a -> RelExpr
p || q = RBinOp Or p q

nEq :: Expressible a => a -> a -> RelExpr
nEq = RBinOp NEq

(!==) :: Expressible a => a -> a -> RelExpr
(!==) = nEq

not' :: Expressible a => a -> RelExpr
not' = RUnOp Neg

-------------------------------------------------------------------------------
-- Parsing
-------------------------------------------------------------------------------

type RelExprParser = P.Parser RelExpr

relOperatorTable :: [[P.Operator P.Parser RelExpr]]
relOperatorTable =
  [ [ P.prefixOp "!" (RUnOp Neg)
    ],
    [ P.infixOpL "!=" (RBinOp NEq),
      P.infixOpL "&&" (RBinOp And),
      P.infixOpL "||" (RBinOp Or)
    ]
  ]

pRelExpr :: Reducible a => P.Parser a -> RelExprParser
pRelExpr pExpr = P.makeExprParser pRelTerm relOperatorTable
  where
    -- pRelTerm :: RelExprParser
    pRelTerm = P.choice [pExpr, pRTernOp]

    pCond :: P.Parser ((TernOp -> a -> a -> a -> RelExpr) -> RelExpr)
    pCond = do
      P.reserved "if"
      x <- pExpr
      P.reserved "then"
      y <- pExpr
      P.reserved "else"
      z <- pExpr
      pure $ \mkC -> mkC Cond x y z

    pRTernOp = do
      c <- pCond
      pure $ c RTernOp

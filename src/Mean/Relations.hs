{-# LANGUAGE GADTs, OverloadedStrings #-}

module Mean.Relations where

import Debug.Trace (traceM)
import Mean.Core hiding (BinOp)
import Mean.Viz
import qualified Mean.Parser as P
import Text.PrettyPrint
    ( Doc, (<+>), char, text )
import Prelude hiding ((&&), (||))
import qualified Prelude as Prel
import Control.Monad.Except ( MonadError(throwError) )

data UnOp = Neg deriving (Prel.Eq)

data BinOp = NEq | And | Or deriving (Prel.Eq)

data RelExpr a where
  RUnOp :: Expressible a => UnOp -> a -> RelExpr a
  RBinOp :: Expressible a => BinOp -> a -> a -> RelExpr a

instance Prel.Eq (RelExpr a) where
  (RUnOp op a) == (RUnOp op' a') = op == op' Prel.&& a == a'
  (RBinOp op a0 a1) == (RBinOp op' a0' a1') = op == op' Prel.&& [a0, a1] == [a0', a1']

instance Pretty (RelExpr a) where
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

instance Show (RelExpr a) where
  show = show . ppr 0

instance Reducible (RelExpr a) where
  reduce expr = case expr of
    RUnOp Neg e -> do
      e' <- reduce e
      pure $ mkCBool $ not $ bool e'
    RBinOp op e0 e1 -> do
      e0' <- reduce e0
      e1' <- reduce e1
      pure $
        mkCBool $ case op of
          NEq -> e0' /= e1'
          And -> bool e0' Prel.&& bool e1'
          Or -> bool e0' Prel.|| bool e1'

-------------------------------------------------------------------------------
-- Parsing
-------------------------------------------------------------------------------
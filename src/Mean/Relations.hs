{-# LANGUAGE GADTs, FlexibleContexts, FlexibleInstances #-}

module Mean.Relations where

import Mean.Core
import Prelude hiding ((&&), (||))
import qualified Prelude as Prel
import Control.Monad.Except ( MonadError(throwError) )

data UnOp = Neg deriving (Eq, Ord)

data BinOp = NEq | And | Or deriving (Eq, Ord)

data TernOp = Cond deriving (Eq, Ord)

data RelExpr where
  RUnOp :: Reducible a => UnOp -> a -> RelExpr
  RBinOp :: Reducible a => BinOp -> a -> a -> RelExpr
  RTernOp :: Reducible a => TernOp -> a -> a -> a -> RelExpr

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

(?) :: Reducible a => a -> a -> (a -> RelExpr)
(?) = RTernOp Cond

(>) :: (a -> RelExpr) -> a -> RelExpr
e > e' = e e'

(&&) :: Reducible a => a -> a -> RelExpr
p && q = RBinOp And p q

(||) :: Reducible a => a -> a -> RelExpr
p || q = RBinOp Or p q

nEq :: Reducible a => a -> a -> RelExpr
nEq = RBinOp NEq

(!==) :: Reducible a => a -> a -> RelExpr
(!==) = nEq

not' :: Reducible a => a -> RelExpr
not' = RUnOp Neg
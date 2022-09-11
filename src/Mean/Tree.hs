module Mean.Tree
  ( T.Tree (..),
    T.drawTree,
    module Mean.Tree
  )
where

import qualified Data.Tree.Binary.Preorder as T
import Mean.Core
import Mean.Viz
import qualified Mean.Parser as P
import Prelude hiding ((*), (~>))

mkChurchTree :: Expressible a => T.Tree a -> Evaluation CoreExpr
mkChurchTree t = case t of
  T.Leaf -> pure leaf
  T.Node e l r -> do
    e' <- reduce e
    l' <- mkChurchTree l
    r' <- mkChurchTree r
    pure $ node * e' * l' * r'
  where
    -- λeλb . e
    leaf = e ~> (b ~> e)
    -- λxλlλrλeλb . b(x)(l e b)(r e b)
    node = x ~> (l ~> (r ~> (e ~> (b ~> (b * x * (l * e * b) * (r * e * b))))))

instance Expressible a => Reducible (T.Tree a) where
  reduce = mkChurchTree

instance Show a => Pretty (T.Tree a) where
  ppr p e = text $ show e
-------------------------------------------------------------------------------
-- Parsing
-------------------------------------------------------------------------------

pTree :: Expressible a => P.Parser a -> P.Parser (T.Tree a)
pTree pExpr =
  P.choice
    [P.try pLeafNode, P.try pUnNode, pBiNode]
  where
    mkLeafNode e = T.Node e T.Leaf T.Leaf
    mkUnNode e l = T.Node e l T.Leaf

    pLeafNode = P.brackets $ mkLeafNode <$> pExpr
    pUnNode = P.brackets $ do
      e <- pExpr
      mkUnNode e <$> pTree pExpr

    pBiNode = P.brackets $ do
      e <- pExpr
      l <- pTree pExpr
      T.Node e l <$> pTree pExpr
module Mean.Tree
  ( T.Tree (..),
    T.drawTree,
    module Mean.Sugar.Syntax,
  )
where

import qualified Data.Tree.Binary.Preorder as T
import qualified Mean.Core

import qualified Data.Set as Set

mkChurchTree :: Reducible a => T.Tree a -> a
mkChurchTree t = case t of
  T.Leaf -> leaf
  T.Node e l r -> node * e * churchTree l * churchTree r
  where
    e = mkSVar "e"
    b = mkSVar "b"
    x = mkSVar "x"
    l = mkSVar "l"
    r = mkSVar "r"

    -- λeλb . e
    leaf = e ~> (b ~> e)
    -- λxλlλrλeλb . b(x)(l e b)(r e b)
    node = x ~> (l ~> (r ~> (e ~> (b ~> (b * x * (l * e * b) * (r * e * b))))))

instance Reducible a => Reducible (T.Tree a) where
  reduce = pure . mkChurchTree
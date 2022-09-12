module Mean.Tree.EvaluationSpec where

import qualified Mean.Core as Core
import Mean.Tree
import Mean.Syntax
import Test.Hspec
import Prelude hiding ((*))

e = mkEVar "e"
b = mkEVar "b"
l = mkEVar "l"
r = mkEVar "r"

spec :: Spec
spec = do
  let t = ETree (Node f (Node (x ~> x) Leaf Leaf) (Node y Leaf Leaf))

  describe "alpha equivalence (@=)" $ do
    it "equates alpha equivalent church trees containing alpha equivalent expression nodes" $ do
      let (Right t0) = Core.eval (ETree (Node (x ~> x) Leaf Leaf))
      let (Right t1) = Core.eval (ETree (Node (y ~> y) Leaf Leaf))

      t0 Core.@= t1 `shouldBe` True

  describe "eval" $ do
    -- todo: formalize lifting of constituent syntax into top level
    let leaf = e ~> (b ~> e)
    let node = x ~> (l ~> (r ~> (e ~> (b ~> (b * x * (l * e * b) * (r * e * b))))))

    it "reduces trees to their church encodings" $ do
      Core.eval t `shouldBe` Core.eval (node * f * (node * (x ~> x) * leaf * leaf) * (node * y * leaf * leaf))

    it "reduces folds over trees" $ do
      -- FA composition, l(r)
      -- λxλlλrλz . (l x) (r x)
      let faLR = x ~> (l ~> (r ~> (z ~> ((l * x) * (r * x)))))
      Core.eval (t * leaf * faLR) `shouldBe` Core.eval (z ~> ((x ~> x) * y))

      -- FA composition, r(l)
      -- λxλlλrλz . (r x) (l x)
      let faRL = x ~> (l ~> (r ~> (z ~> ((r * x) * (l * x)))))
      let t' = ETree (Node f (Node y Leaf Leaf) (Node (x ~> x) Leaf Leaf))
      Core.eval (t' * leaf * faRL) `shouldBe` Core.eval (z ~> ((x ~> x) * y))

      -- PA composition, l(r)
      let paLR = faLR
      let t' = ETree (Node f (Node (mkEBind y) Leaf Leaf) (Node (x ~> x) Leaf Leaf))
      Core.eval (t' * leaf * paLR) `shouldBe` Core.eval (z ~> (y ~> (x ~> x)))

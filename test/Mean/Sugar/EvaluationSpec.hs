module Mean.Sugar.EvaluationSpec where

import Debug.Trace (traceM)
import Mean.Core.Syntax hiding ((*), (~>))
import qualified Mean.Core.Syntax as CSyn
import Mean.Core.Viz
import qualified Mean.Sugar.Syntax as SSyn
import Mean.Sugar.Syntax hiding ((*=), (@=))
import Test.HUnit ((@?=))
import Test.Hspec
import Prelude hiding (id, (&&), (*), (-), (||))

e0 @= e1 = (e0 CSyn.@= e1) @?= True

e0 *= e1 = (e0 SSyn.*= e1) @?= True


f :: SugarExpr
f = mkSVar "f"

x :: SugarExpr
x = mkSVar "x"

y :: SugarExpr
y = mkSVar "y"

z :: SugarExpr
z = mkSVar "z"

e :: SugarExpr
e = mkSVar "e"

b :: SugarExpr
b = mkSVar "b"

l :: SugarExpr
l = mkSVar "l"

r :: SugarExpr
r = mkSVar "r"

spec :: Spec
spec = do
  let s = mkSVar "s"
  let t = STree (Node s (Node (x ~> x) Leaf Leaf) (Node y Leaf Leaf))

  describe "alpha equivalence (@=)" $ do
    it "equates alpha equivalent church trees containing alpha equivalent expression nodes" $ do
      let (Right t0) = eval (STree (Node (x ~> x) Leaf Leaf))
      let (Right t1) = eval (STree (Node (y ~> y) Leaf Leaf))

      t0 @= t1

    it "equates sets containing alpha equivalent items" $ do
      let (Right s0) = eval (SSet [y ~> y])
      let (Right s1) = eval (SSet [x ~> x])

      s0 @= s1

  describe "eval" $ do
    it "reduces trees to their church encodings" $ do
      eval t `shouldBe` eval (node * s * (node * (x ~> x) * leaf * leaf) * (node * y * leaf * leaf))

    it "reduces folds over trees" $ do
      -- FA composition, l(r)
      -- λxλlλrλz . (l x) (r x)
      let faLR = x ~> (l ~> (r ~> (z ~> ((l * x) * (r * x)))))
      eval (t * leaf * faLR) `shouldBe` eval (z ~> ((x ~> x) * y))

      -- FA composition, r(l)
      -- λxλlλrλz . (r x) (l x)
      let faRL = x ~> (l ~> (r ~> (z ~> ((r * x) * (l * x)))))
      let t' = STree (Node s (Node y Leaf Leaf) (Node (x ~> x) Leaf Leaf))
      eval (t' * leaf * faRL) `shouldBe` eval (z ~> ((x ~> x) * y))

      -- PA composition, l(r)
      let paLR = faLR
      let t' = STree (Node s (Node (mkSBind y) Leaf Leaf) (Node (x ~> x) Leaf Leaf))
      eval (t' * leaf * paLR) `shouldBe` eval (z ~> (y ~> (x ~> x)))

    it "reduces case expressions" $ do
      eval (SCase true [(true, x)]) `shouldBe` Right CSyn.x
      eval (SCase false [(false, y), (true, x)]) `shouldBe` Right CSyn.y
      eval (SCase y [((x ~> x) * y, (x ~> x) * y), (false, x), (true, z)]) `shouldBe` Right CSyn.y

    it "reduces set expressions to characteristic functions" $ do
      let s = SSet [e, b, f, x]

      eval (s * e) `shouldBe` Right CSyn.true
      eval (s * z) `shouldBe` Right CSyn.false

      eval ((s * e) || (s * z)) `shouldBe` Right CSyn.true
      eval ((s * e) && (s * z)) `shouldBe` Right CSyn.false

      eval ((s * e) || (s * b)) `shouldBe` Right CSyn.true
      eval ((s * e) && (s * b)) `shouldBe` Right CSyn.true

-- it "reduces set comprehensions to characteristic functions" $ do

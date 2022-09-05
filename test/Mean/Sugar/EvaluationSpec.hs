module Mean.Sugar.EvaluationSpec where

import Debug.Trace (traceM)
import qualified Mean.Core.Encoding as CEnc
import qualified Mean.Core.Evaluation as CEval
import Mean.Core.Syntax hiding ((*), (~>))
import qualified Mean.Core.Syntax as CSyn
import Mean.Core.Viz
import Mean.Sugar.Encoding
import Mean.Sugar.Evaluation hiding ((*=), (@=))
import qualified Mean.Sugar.Evaluation as SEval
import Mean.Sugar.Syntax
import Test.HUnit ((@?=))
import Test.Hspec
import Prelude hiding (id, (&&), (*), (-))

e0 @= e1 = (e0 CEval.@= e1) @?= True

e0 *= e1 = (e0 SEval.*= e1) @?= True

spec :: Spec
spec = do
  let s = mkSVar "s"
  let t = STree (Node s (Node (x ~> x) Leaf Leaf) (Node y Leaf Leaf))

  describe "alpha equivalence (@=)" $ do
    it "equates alpha equivalent church trees containing alpha equivalent expression nodes" $ do
      let (Right t0) = eval (STree (Node (x ~> x) Leaf Leaf))
      let (Right t1) = eval (STree (Node (y ~> y) Leaf Leaf))

      t0 @= t1

  describe "eval" $ do
    it "reduces sugar trees to their church encodings" $ do
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
      eval (SCase true [(true, x)]) `shouldBe` Right CEnc.x
      eval (SCase false [(false, y), (true, x)]) `shouldBe` Right CEnc.y
      eval (SCase y [((x ~> x) * y, (x ~> x) * y), (false, x), (true, z)]) `shouldBe` Right CEnc.y

    it "reduces set expressions to characteristic functions" $ do
      let s = SSet [e, b, f, x]

      eval (s * e) `shouldBe` Right CEnc.true
      eval (s * b) `shouldBe` Right CEnc.true
      eval (s * f) `shouldBe` Right CEnc.true
      eval (s * x) `shouldBe` Right CEnc.true
      eval (s * z) `shouldBe` Right CEnc.false

-- it "reduces set expressions" $ do

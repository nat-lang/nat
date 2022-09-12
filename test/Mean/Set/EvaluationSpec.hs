module Mean.Set.EvaluationSpec where

import Debug.Trace (traceM)
import qualified Mean.Core as Core
import Mean.Set
import Mean.Syntax
import Test.Hspec
import Prelude hiding ((*))

spec :: Spec
spec = do
  describe "alpha equivalence (@=)" $ do
    it "equates sets containing alpha equivalent items" $ do
      let (Right s0) = Core.eval (ESet (Set [x ~> x]))
      let (Right s1) = Core.eval (ESet (Set [y ~> y]))

      s0 Core.@= s1 `shouldBe` True

  describe "eval" $ do
    it "reduces set expressions to characteristic functions" $ do
      let s = ESet $ Set [x,y,z,f]

      Core.eval (s * x) `shouldBe` Right Core.true
      Core.eval (s * y) `shouldBe` Right Core.true
      Core.eval (s * z) `shouldBe` Right Core.true
      Core.eval (s * f) `shouldBe` Right Core.true

      let b = mkEVar "b"

      Core.eval (s * b) `shouldBe` Right Core.false
      Core.eval (s * s) `shouldBe` Right Core.false

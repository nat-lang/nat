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
      let i = ELit . Core.LInt
      let s = ESet $ Set [i 0, i 1, i 2, i 3]

      Core.eval (s * i 0) `shouldBe` Right Core.true
      Core.eval (s * i 1) `shouldBe` Right Core.true
      Core.eval (s * i 2) `shouldBe` Right Core.true
      Core.eval (s * i 3) `shouldBe` Right Core.true

      Core.eval (s * i 4) `shouldBe` Right Core.false

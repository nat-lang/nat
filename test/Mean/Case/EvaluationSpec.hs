module Mean.Case.EvaluationSpec where

import qualified Mean.Core as Core
import Mean.Case
import Mean.Syntax
import Test.Hspec
import Prelude hiding ((*))

zero = ELit Core.lZero
one = ELit Core.lOne

spec :: Spec
spec = do
  describe "eval" $ do
    it "reduces case expressions on booleans" $ do
      Core.eval (ECase (Case true [(true, x)])) `shouldBe` Right Core.x
      Core.eval (ECase (Case false [(false, y), (true, x), (false, z)])) `shouldBe` Right Core.y

    it "reduces case expressions on terms that reduce to booleans" $ do
      Core.eval (ECase (Case one [((x ~> x) * one, one), (zero, x), (one, z)])) `shouldBe` Right Core.one

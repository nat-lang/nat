module Mean.Case.EvaluationSpec where

import qualified Mean.Core as Core
import Mean.Case
import Mean.Syntax
import Test.Hspec
import Prelude hiding ((*))

spec :: Spec
spec = do
  describe "eval" $ do
    it "reduces case expressions" $ do
      Core.eval (ECase (Case true [(true, x)])) `shouldBe` Right Core.x
      Core.eval (ECase (Case false [(false, y), (true, x), (false, z)])) `shouldBe` Right Core.y
      Core.eval (ECase (Case y [((x ~> x) * y, (x ~> x) * y), (z, x), (y, z)])) `shouldBe` Right Core.y

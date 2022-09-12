module Mean.Syntax.EvaluationSpec where

import Debug.Trace (traceM)
import qualified Mean.Core as Core
import Mean.Set
import Mean.Case
import Mean.Relations
import Mean.Syntax hiding ((*=), (@=))
import Test.HUnit ((@?=))
import Test.Hspec
import Prelude hiding (id, (&&), (*), (||), (>))

a = mkEVar "a"
b = mkEVar "b"
l = mkEVar "l"
r = mkEVar "r"

spec :: Spec
spec = do
  describe "eval" $ do
    it "reduces truth conditional binary operations between operations on sets" $ do
      let i = ELit . Core.LInt
      let s = ESet (Set [i 0, i 1])

      Core.eval ((s * i 0) || (s * i 2)) `shouldBe` Right Core.true
      Core.eval ((s * i 0) && (s * i 2)) `shouldBe` Right Core.false

      Core.eval ((s * i 0) || (s * i 1)) `shouldBe` Right Core.true
      Core.eval ((s * i 0) && (s * i 1)) `shouldBe` Right Core.true

module Mean.Syntax.EvaluationSpec where

import Debug.Trace (traceM)
import qualified Mean.Core as Core
import Mean.Set
import Mean.Case
import Mean.Relations
import Mean.Syntax hiding ((*=), (@=))
import Test.HUnit ((@?=))
import Test.Hspec
import Prelude hiding (id, (&&), (*), (||))

a = mkEVar "a"
b = mkEVar "b"

spec :: Spec
spec = do
  describe "eval" $ do
    it "reduces truth conditional binary operations between operations on sets" $ do
      let s = ESet (Set [a,b])

      Core.eval ((s * a) || (s * z)) `shouldBe` Right Core.true
      Core.eval ((s * a) && (s * z)) `shouldBe` Right Core.false

      Core.eval ((s * a) || (s * b)) `shouldBe` Right Core.true
      Core.eval ((s * a) && (s * b)) `shouldBe` Right Core.true


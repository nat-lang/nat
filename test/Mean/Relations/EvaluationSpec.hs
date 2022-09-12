module Mean.Relations.EvaluationSpec where

import qualified Mean.Core as Core
import Mean.Relations
import Mean.Syntax
import Test.Hspec
import Prelude hiding ((*), (>), (&&), (||))

zero = ELit Core.lZero
one = ELit Core.lOne

spec :: Spec
spec = do
  describe "eval" $ do
    let id' y = (x ~> x) * y

    it "reduces truth conditional unary operations on booleans" $ do
      Core.eval (not' true) `shouldBe` Right Core.false
      Core.eval (not' false) `shouldBe` Right Core.true

    it "reduces truth conditional binary operations on booleans" $ do
      Core.eval (true && false) `shouldBe` Right Core.false
      Core.eval (true || false) `shouldBe` Right Core.true

    it "reduces truth conditional unary operations on function applications" $ do
      Core.eval (not' (id' true)) `shouldBe` Right Core.false
      Core.eval (not' (id' false)) `shouldBe` Right Core.true

    it "reduces truth conditional binary operations on function applications" $ do
      Core.eval (id' true && id' false) `shouldBe` Right Core.false
      Core.eval (id' true || id' false) `shouldBe` Right Core.true

    it "reduces inequalities between primitives" $ do
      Core.eval (true !== false) `shouldBe` Right Core.true
      Core.eval (x !== y) `shouldBe` Right Core.true
      Core.eval (zero !== one) `shouldBe` Right Core.true

    it "reduces inequalities between relations" $ do
      Core.eval ((true && true) !== (false && false)) `shouldBe` Right Core.true

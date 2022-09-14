module Mean.Syntax.EvaluationSpec where

import Debug.Trace (traceM)
import qualified Mean.Core as Core
import Mean.Set
import Mean.Case
import Mean.Relations
import Mean.Syntax hiding ((*=), (@=))
import Test.HUnit ((@?=))
import Test.Hspec
import Prelude hiding (id, (&&), (*), (||), (>), LTE)

a = mkEVar "a"
b = mkEVar "b"
l = mkEVar "l"
r = mkEVar "r"

spec :: Spec
spec = do
  describe "eval" $ do
    let iE = ELit . Core.LInt
    let iC = Core.CLit . Core.LInt

    it "reduces truth conditional binary operations between operations on sets" $ do
      let s = ESet (Set [iE 0, iE 1])

      Core.eval ((s * iE 0) || (s * iE 2)) `shouldBe` Right Core.true
      Core.eval ((s * iE 0) && (s * iE 2)) `shouldBe` Right Core.false

      Core.eval ((s * iE 0) || (s * iE 1)) `shouldBe` Right Core.true
      Core.eval ((s * iE 0) && (s * iE 1)) `shouldBe` Right Core.true

    it "reduces applications of recursive functions" $ do
      let n = mkEVar "n"
      --  λn. if n ≤ 1 then 1 else n * fact (n - 1)
      let fact = ELetRec (Core.mkVar "a") (n ~> EBinOp Core.LTE n (iE 1) ? iE 1 > EBinOp Core.Mul n (a * EBinOp Core.Sub n (iE 1)))

      Core.eval (fact * iE 0) `shouldBe` Right (iC 1)
      Core.eval (fact * iE 1) `shouldBe` Right (iC 1)

      Core.eval (fact * iE 2) `shouldBe` Right (iC 2)
      Core.eval (fact * iE 3) `shouldBe` Right (iC 6)
      Core.eval (fact * iE 4) `shouldBe` Right (iC 24)
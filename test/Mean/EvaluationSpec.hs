{-# LANGUAGE OverloadedStrings #-}

module Mean.EvaluationSpec where

import Debug.Trace (traceM)
import Mean.Evaluation
import Mean.Parser
import Mean.Syntax
import Mean.Viz
import Test.Hspec

spec :: Spec
spec =
  let fn x y = Lam (Binder (mkVar x) TyNil) y
      f = mkEVar "f"
      x = mkEVar "x"
      y = mkEVar "y"
      m = mkEVar "m"
      n = mkEVar "n"
      -- λfλx . x
      zero = fn "f" (fn "x" x)
      -- λfλx . f x
      one = fn "f" (fn "x" (App f x))
      -- λfλx . f(f x)
      two = fn "f" (fn "x" (App f (App f x)))
      -- λfλx . f(f(f x))
      three = fn "f" (fn "x" (App f (App f (App f x))))
      -- λnλfλx . f(f x)
      succ = fn "n" (fn "f" (fn "x" (App f (App (App n f) x))))
      -- λmλn . m succ n
      add = fn "m" (fn "n" (App (App m succ) n))
      -- λmλnλfλx . m f (n f x)
      add' = fn "m" (fn "n" (fn "f" (fn "x" (App (App m f) (App (App n f) x)))))
      -- λmλn . m(add' n)0
      mul = fn "m" (fn "n" (App (App m (App add' n)) zero))
      -- λmλn . m(mul n)1
      exp = fn "m" (fn "n" (App (App m (App mul n)) one))
      -- λxλy . x
      true = fn "x" (fn "y" x)
      -- λxλy . y
      false = fn "x" (fn "y" y)
      -- λfλxλy . f(x)(y)
      if' = fn "f" (fn "x" (fn "y" (App (App f x) y)))
      -- λxλy . if(x)(y)(false)
      and' = fn "x" (fn "y" (App (App (App if' x) y) false))
      -- λxλy . if(x)(true)(y)
      or' = fn "x" (fn "y" (App (App (App if' x) true) y))
      -- λxλy . if(x)(true)(false)
      not' = fn "x" (fn "y" (App (App (App if' x) true) false))
      -- λx.x
      id' = fn "x" x
   in do
        describe "alphaEq (@=)" $ do
          it "recognizes alpha equivalence" $ do
            fn "x" x @= fn "y" y `shouldBe` True

        describe "eval" $ do
          it "performs beta reduction" $ do
            runEval (App id' zero) `shouldBe` Right zero
            runEval (App id' id') `shouldBe` Right id'

            runEval (App succ one) `shouldBe` Right two
            runEval (App succ (App succ one)) `shouldBe` Right three

            runEval (App (App mul zero) one) `shouldBe` Right zero
            runEval (App (App mul one) three) `shouldBe` Right three

            runEval (App (App add' two) zero) `shouldBe` Right two
            runEval (App (App add' zero) three) `shouldBe` Right three
            runEval (App (App add' two) one) `shouldBe` Right three
            runEval (App (App add' one) two) `shouldBe` Right three

            runEval (App (App add two) zero) `shouldBe` Right two
            runEval (App (App add zero) three) `shouldBe` Right three
            runEval (App (App add two) one) `shouldBe` Right three
            runEval (App (App add one) two) `shouldBe` Right three

            runEval (App (App or' true) false) `shouldBe` Right true
            runEval (App (App and' true) false) `shouldBe` Right false

{-

need to test:

(1) safe sub when nested binder conflicts with higher binder
(2) safe sub when nested binder conflicts with free vars of contractum
(3) alpha equivalence, more cases
(4) confluence

-}

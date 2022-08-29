{-# LANGUAGE OverloadedStrings #-}

module Mean.EvaluationSpec where

import Debug.Trace (traceM)
import Mean.Evaluation hiding ((*=), (@=))
import qualified Mean.Evaluation as E
import Mean.Parser
import Mean.Syntax
import Mean.Viz
import Test.HUnit ((@?=))
import Test.Hspec
import Prelude hiding (and, id, not, or, (&&), (+), (-), (||))

e0 @= e1 = (e0 E.@= e1) @?= True

e0 @!= e1 = (e0 E.@= e1) @?= False

e0 *= e1 = (e0 E.*= e1) @?= True

spec :: Spec
spec =
  let fn x y = Lam (Binder (mkVar x) TyNil) y
      f = mkEVar "f"
      x = mkEVar "x"
      y = mkEVar "y"
      m = mkEVar "m"
      n = mkEVar "n"
      -- λx.x
      id = fn "x" x
      -- λnλfλx . f(n f x)
      succ = fn "n" (fn "f" (fn "x" (App f (App (App n f) x))))
      -- λfλx . x
      zero = fn "f" (fn "x" x)
      -- λfλx . f x
      one = fn "f" (fn "x" (App f x))
      -- λfλx . f(f x)
      two = fn "f" (fn "x" (App f (App f x)))
      -- λfλx . f(f(f x))
      three = fn "f" (fn "x" (App f (App f (App f x))))
      -- λmλn . m succ n
      add = fn "m" (fn "n" (App (App m succ) n))
      m + n = App (App add m) n
      -- λmλnλfλx . m f (n f x)
      add' = fn "m" (fn "n" (fn "f" (fn "x" (App (App m f) (App (App n f) x)))))
      m ++ n = App (App add' m) n
      -- λmλn . m(add' n)0
      mul = fn "m" (fn "n" (App (App m (App add n)) zero))
      m * n = App (App mul m) n
      -- λmλn . m(mul n)1
      exp = fn "m" (fn "n" (App (App n (App mul m)) one))
      m ** n = App (App exp m) n
      -- λxλy . x
      true = fn "x" (fn "y" x)
      -- λxλy . y
      false = fn "x" (fn "y" y)
      -- λfλxλy . f(x)(y)
      if' = fn "f" (fn "x" (fn "y" (App (App f x) y)))
      x ~> y = App (App if' x) y
      -- λxλy . if(x)(y)(false)
      and = fn "x" (fn "y" (App (App (App if' x) y) false))
      x && y = App (App and x) y
      -- λxλy . if(x)(true)(y)
      or = fn "x" (fn "y" (App (App (App if' x) true) y))
      x || y = App (App or x) y
      -- λxλy . if(x)(true)(false)
      not = fn "x" (fn "y" (App (App (App if' x) true) false))
      not' x = App not x
      (-) = not'
   in do
        describe "sub" $ do
          it "substitutes expressions for variables" $ do
            let z = mkVar "z"
            sub y z (EVar z) `shouldBe` y

          it "does so discriminately" $ do
            let z = mkVar "z"
            sub y z x `shouldBe` x

          it "avoids variable capture" $ do
            let fn' x y = Lam (Binder x TyNil) y
            let f1 = Var "f0" "f1"

            -- there are two possible conflicts for a substitution e'[e/v]:
            --  (1) a nested binder conflicts with v, as in
            --        (λf . f)[x/f]
            --     in which case we want λf . f rather than λf . x
            --  (2) a nested binder conflicts with a free variable in e, as in
            --        (λf . f n)[f/n]
            --     in which case we want λf1 . f1 f rather than λf . f f

            -- (1)
            sub x (mkVar "f") (fn "f" f) `shouldBe` fn "f" f
            -- (2)
            sub f (mkVar "n") (fn "f" (App f n)) `shouldBe` fn' f1 (App (EVar f1) f)

        describe "alphaEq (@=)" $ do
          it "recognizes alpha equivalence" $ do
            fn "x" x @= fn "y" y
            -- λfλx . f(x) == λfλy . f(y)
            fn "f" (fn "x" (App f x)) @= fn "f" (fn "y" (App f y))
            -- λfλx . x(f) == λfλx . y(f)
            fn "f" (fn "x" (App x f)) @= fn "f" (fn "y" (App y f))
          it "recognizes syntactic equivalence" $ do
            x @= x
            id @= id
            fn "x" x @= fn "x" x
          it "doesn't recognize anything else" $ do
            x @!= y
            zero @!= one
            -- λfλx . f(x) != λxλf . f(x)
            fn "f" (fn "x" (App f x)) @!= fn "x" (fn "f" (App f x))
            -- λfλx . f(x) != λfλx . x(f)
            fn "f" (fn "x" (App f x)) @!= fn "f" (fn "x" (App x f))

        describe "eval" $ do
          it "does elementary beta reduction" $ do
            App id id *= id

          it "reduces church numerals" $ do
            (one + zero) *= one
            (zero + two) *= two
            (two + one) *= three

            (one ++ zero) *= one
            (zero ++ two) *= two
            (two ++ one) *= three

            (zero * one) *= zero
            (one * three) *= three
            (two * three) *= (three + three)

            (two ** one) *= two
            (two ** two) *= (two * two)
            (two ** three) *= (two * (two * two))

          it "reduces booleans" $ do
            (true || false) *= true
            (false || true) *= true
            (true || true) *= true
            (false || false) *= false

            (true && false) *= false
            (false && true) *= false
            (false && false) *= false
            (true && true) *= true

{-
can we get something like these to work?

let (Right ifXThenY) = eval (App (App if' x) y)
let (Right notXAndNotY) = eval (App not (App (App and x) (App not y)))
let (Right yOrNotX) = eval (App (App or (App not x)) y)

ifXThenY `shouldBe` notXAndNotY
ifXThenY `shouldBe` yOrNotX
let (Right notXAndY) = eval (App not (App (App and x) y))
let (Right notXOrNotY) = eval (App (App or (App not x)) (App not y))
notXAndY `shouldBe` notXOrNotY
eval (not' (x && y)) `shouldBe` eval (or' (not' x) (not' y))
eval (or' (not' x) (not' y)) `shouldBe` eval (not' (x && y))
-}

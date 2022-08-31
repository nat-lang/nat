{-# LANGUAGE OverloadedStrings #-}

module Mean.Core.EvaluationSpec where

import Debug.Trace (traceM)
import Mean.Core.Evaluation hiding ((*=), (@=))
import qualified Mean.Core.Evaluation as E
import Mean.Core.Encoding
import Mean.Core.Parser
import Mean.Core.Syntax
import Mean.Core.Viz
import Test.HUnit ((@?=))
import Test.Hspec
import Prelude hiding (and, exp, id, not, or, succ, (&&), (*), (**), (+), (++), (-), (||))

e0 @= e1 = (e0 E.@= e1) @?= True

e0 @!= e1 = (e0 E.@= e1) @?= False

e0 *= e1 = (e0 E.*= e1) @?= True

spec :: Spec
spec = do
  describe "substitution" $ do
    it "substitutes expressions for variables" $ do
      sub y z z `shouldBe` y

    it "does so discriminately" $ do
      sub y z x `shouldBe` x

    it "avoids variable capture" $ do
      let f1 = CVar $ Var "f0" "f1"

      -- there are two possible conflicts for a substitution e'[e/v]:
      --  (1) a nested binder conflicts with v, as in
      --        (λf . f)[x/f]
      --     in which case we want (λf . f) rather than (λf . x)
      sub x f (f ~> f) `shouldBe` f ~> f
  
      --  (2) a nested binder conflicts with a free variable in e, as in
      --        (λf . f n)[f/n]
      --     in which case we want (λf1 . f1 f) rather than (λf . f f)
      sub f n (f ~> (f * n)) `shouldBe` f1 ~> (f1 * f)

  describe "alpha equivalence (@=)" $ do
    it "recognizes alpha equivalence" $ do
      (x ~> x) @= (y ~> y)
      -- λfλx . f(x) == λfλy . f(y)
      (f ~> (x ~> (f * x))) @= (f ~> (y ~> (f * y)))
      -- λfλx . x(f) == λfλx . y(f)
      (f ~> (x ~> (x * f))) @= (f ~> (y ~> (y * f)))
    it "recognizes syntactic equivalence" $ do
      -- this is just a special case of alpha equivalence,
      -- but probably @= should check for syntactic equivalence
      -- before trying substitutions for the sake of efficiency. tbc
      x @= x
      id @= id
      (x ~> x) @= (x ~> x)
    it "doesn't recognize anything else" $ do
      x @!= y
      zero @!= one
      -- λfλx . f(x) != λxλf . f(x)
      (f ~> (x ~> (f * x))) @!= (x ~> (f ~> (f * x)))
      -- λfλx . f(x) != λfλx . x(f)
      (f ~> (x ~> (f * x))) @!= (f ~> (x ~> (x * f)))

  describe "confluence (*=)" $ do
    it "equates lambda expressions" $ do
      id * id *= id

    it "equates peano numerals" $ do
      succ * one *= two
      succ * (succ * one) *= three

    it "equates church numerals" $ do
      let m * n = mkCApp (mkCApp mul m) n

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

    it "equates church booleans" $ do
      (true || false) *= true
      (false || true) *= true
      (true || true) *= true
      (false || false) *= false

      (true && false) *= false
      (false && true) *= false
      (false && false) *= false
      (true && true) *= true

{-
can we get de morgan to work with the church encodings?

  -(x && y) *= -x || -y
  -x || -y *= -(x && y)

  (if x y) *= -(x && -y)
  (if x y) *= (-x || y)
-}

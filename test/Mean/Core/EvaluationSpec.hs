{-# LANGUAGE OverloadedStrings #-}

module Mean.Core.EvaluationSpec where

import Debug.Trace (traceM)
import Mean.Core.Encoding
import Mean.Core.Parser
import Mean.Core.Syntax hiding ((@=), (*=))
import qualified Mean.Core.Syntax as S
import Mean.Core.Viz
import Test.HUnit ((@?=))
import Test.Hspec
import Prelude hiding (and, exp, id, not, or, succ, (!), (&&), (*), (**), (+), (++), (-), (>), (||))

e0 @= e1 = (e0 S.@= e1) @?= True

e0 !@= e1 = (e0 S.@= e1) @?= False

e0 *= e1 = (e0 S.*= e1) @?= True

spec :: Spec
spec = do
  let zero = CLit lZero
  let one = CLit lOne

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
      x !@= y
      zero !@= one
      -- λfλx . f(x) != λxλf . f(x)
      (f ~> (x ~> (f * x))) !@= (x ~> (f ~> (f * x)))
      -- λfλx . f(x) != λfλx . x(f)
      (f ~> (x ~> (f * x))) !@= (f ~> (x ~> (x * f)))

  describe "eval" $ do
    let id' y = (x ~> x) * y

    it "reduces truth conditional unary operations on booleans" $ do
      eval (not' true) `shouldBe` Right false
      eval (not' false) `shouldBe` Right true
    it "reduces truth conditional unary operations on function applications" $ do
      eval (not' (id' true)) `shouldBe` Right false
      eval (not' (id' false)) `shouldBe` Right true

    it "reduces truth conditional binary operations on booleans" $ do
      eval (true && false) `shouldBe` Right false
      eval (true || false) `shouldBe` Right true
    it "reduces truth conditional binary operations on function applications" $ do
      eval (id' true && id' false) `shouldBe` Right false
      eval (id' true || id' false) `shouldBe` Right true

    it "reduces equality relations between primitives" $ do
      eval (true === false) `shouldBe` Right false
      eval (true !== false) `shouldBe` Right true
      eval (x === y) `shouldBe` Right false
      eval (x !== y) `shouldBe` Right true
      eval (zero === zero) `shouldBe` Right true
      eval (zero === one) `shouldBe` Right false
      eval (zero !== one) `shouldBe` Right true
    it "reduces equality relations between expressions" $ do
      eval (id' true === id' false) `shouldBe` Right false
      eval (id' true !== id' false) `shouldBe` Right true

    it "reduces ternary conditionals on booleans" $ do
      eval (false ? l > r) `shouldBe` Right r
      eval (true ? l > r) `shouldBe` Right l
    it "reduces ternary conditionals on function applications" $ do
      eval (((x ~> x) * false) ? l > r) `shouldBe` Right r
      eval (((x ~> x) * true) ? ((x ~> x) * l) > r) `shouldBe` Right l
    it "fails on un truthy conditionals" $ do
      eval (x ? l > r) `shouldBe` Left (S.NotTruthy x)

    it "only evaluates operations on terms whose bound variables have already been substituted?" $ do
      eval (x ~> (x === true ? l > r)) `shouldBe` Right (x ~> (x === true ? l > r))

  describe "confluence (*=)" $ do
    -- λnλfλx . f(n f x)
    let succ = n ~> (f ~> (x ~> (f * (n * f * x))))
    -- λfλx . x
    let zero = f ~> (x ~> x)
    -- λfλx . f x
    let one = f ~> (x ~> (f * x))
    -- λfλx . f(f x)
    let two = f ~> (x ~> (f * (f * x)))
    -- λfλx . f(f(f x))
    let three = f ~> (x ~> (f * (f * (f * x))))
    -- λmλn . m succ n
    let add = m ~> (n ~> (m * succ * n))
    let m + n = add * m * n
    -- λmλnλfλx . m f (n f x)
    let add' = m ~> (n ~> (f ~> (x ~> (m * f * (n * f * x)))))
    let m ++ n = add' * m * n
    -- λmλn . m(add' n)0
    let mul = m ~> (n ~> (m * (add * n) * zero))
    -- λmλn . n(mul m)1
    let exp = m ~> (n ~> (n * (mul * m) * one))
    let m ** n = exp * m * n
    -- λxλy . x
    let true' = x ~> (y ~> x)
    -- λxλy . y
    let false' = x ~> (y ~> y)
    -- λfλxλy . f(x)(y)
    let if' = f ~> (x ~> (y ~> (f * x * y)))
    -- λxλy . if(x)(y)(false)
    let (&&) p q = (x ~> (y ~> (if' * x * y * false'))) * p * q
    -- λxλy . if(x)(true)(y)
    let (||) p q = (x ~> (y ~> (if' * x * true' * y))) * p * q
    -- λxλy . if(x)(true)(false)
    let not' = x ~> (if' * x * false' * true')

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
      (true' || false') *= true'
      (false' || true') *= true'
      (true' || true') *= true'
      (false' || false') *= false'

      (true' && false') *= false'
      (false' && true') *= false'
      (false' && false') *= false'
      (true' && true') *= true'

      not' * (true' || false') *= false'
      not' * (false' || true') *= false'
      not' * (true' || true') *= false'
      not' * (false' || false') *= true'

      not' * (true' && false') *= true'
      not' * (false' && true') *= true'
      not' * (false' && false') *= true'
      not' * (true' && true') *= false'

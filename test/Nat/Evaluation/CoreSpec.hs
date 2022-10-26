{-# LANGUAGE OverloadedStrings #-}

module Nat.Evaluation.CoreSpec where

import qualified Nat.Context as C
import Nat.Evaluation.Core
import Nat.Syntax.Core
import Test.Hspec

spec :: Spec
spec = do
  describe "fv" $ do
    it "returns the set of free variables of an expression" $ do
      True `shouldBe` False
  describe "sub" $ do
    let [(a, a'), (b, b'), (c, c')] = (\c -> (C.var c, var c)) <$> ["a", "b", "c"]

    let e0 = lam a (app a' b')
    let e1 = lam c (app e0 c')

    it "substitutes expressions for variables in expressions" $ do
      C.sub b c' (lam a b') `shouldBe` lam a c'
      C.sub b c' (app a' b') `shouldBe` app a' c'
      C.sub b c' (lam a (app a' b')) `shouldBe` lam a (app a' c')

    it "ignores bound variables" $ do
      C.sub a c' (lam a a') `shouldBe` lam a a'
      C.sub a c' (lam a (app a' b')) `shouldBe` lam a (app a' b')

  describe "reduce" $ do
    it "reduces the combinators" $ do
      True `shouldBe` False
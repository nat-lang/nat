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
    let [(a, a'), (b, b'), (c, c')] = (\c -> (C.var c, EVar $ C.var c)) <$> ["a", "b", "c"]

    let e0 = ELam a (EApp a' b')
    let e1 = ELam c (EApp e0 c')

    it "substitutes expressions for variables in expressions" $ do
      C.sub a b' a' `shouldBe` b'
      C.sub b c' (ELam a b') `shouldBe` ELam a c'
      C.sub b c' (EApp a' b') `shouldBe` EApp a' c'
      C.sub b c' (ELam a (EApp a' b')) `shouldBe` ELam a (EApp a' c')

    it "ignores bound variables" $ do
      C.sub a c' (ELam a a') `shouldBe` ELam a a'
      C.sub a c' (ELam a (EApp a' b')) `shouldBe` ELam a (EApp a' b')

  describe "reduce" $ do
    it "reduces the combinators" $ do
      True `shouldBe` False
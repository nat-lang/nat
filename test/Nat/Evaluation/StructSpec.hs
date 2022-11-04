{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Nat.Evaluation.StructSpec where

import Nat.Evaluation.Struct
import Nat.Evaluation.Surface
import Nat.Parser
import Nat.Syntax.Surface
import Test.Hspec
import Text.RawString.QQ

mkI = ELit . LInt

spec :: Spec
spec = do
  let p = parse pExpr
  -- describe "identity" $ do
  -- describe "inversion" $ do
  -- describe "commutativity" $ do
  -- describe "associativity" $ do
  -- describe "idempotence" $ do
  -- describe "closure" $ do
  -- describe "algebra" $ do
  -- describe "semigroup" $ do
  -- let mod = [r|

  describe "group" $ do
    it "verifies that the integers {0,1,2,3} form a group with the operation of addition mod 4" $ do
      let (Right s) = p [r|{0,1,2,3}|]
      let (Right f) = p [r|\x.\y. (x + y) % 4|]

      is group (Struct s [f]) `shouldBe` Right True

  describe "monoid" $ do
    it "verifies that the integers {0,1,2,3,4} form a monoid with the operation of multiplication mod 5" $ do
      let (Right s) = p [r|{0,1,2,3,4}|]
      let (Right f) = p [r|\x.\y. (x * y) % 5|]

      is monoid (Struct s [f]) `shouldBe` Right True

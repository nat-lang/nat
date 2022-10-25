{-# LANGUAGE OverloadedStrings #-}

module Nat.Evaluation.CoreSpec where

import Test.Hspec

spec :: Spec
spec = do
  describe "fv" $ do
    True `shouldBe` False
  describe "reduce" $ do
    True `shouldBe` False
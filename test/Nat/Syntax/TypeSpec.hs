{-# LANGUAGE OverloadedStrings #-}

module Nat.Syntax.TypeSpec where

import Nat.Parser (parse)
import Nat.Syntax.Type
import Test.Hspec

spec :: Spec
spec = do
  describe "pType" $ do
    it "parses the primitive types" $ do
      parse pType "<n>" `shouldBe` Right tyInt
      parse pType "<t>" `shouldBe` Right tyBool
    it "parses variable types" $ do
      parse pType "<A>" `shouldBe` Right (mkTv "A")
      parse pType "<ZZ>" `shouldBe` Right (mkTv "ZZ")
    it "parses functional types" $ do
      parse pType "<n,t>" `shouldBe` Right (TyFun tyInt tyBool)
      parse pType "<n,<A,t>>" `shouldBe` Right (TyFun tyInt (TyFun (mkTv "A") tyBool))

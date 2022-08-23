{-# LANGUAGE OverloadedStrings #-}

module Mean.ParserSpec where

import Control.Exception (evaluate)
import Mean.Parser (parseTree)
import Mean.Syntax
import Test.Hspec

spec :: Spec
spec = do
  describe "Mean.Parser.parseTree" $ do
    it "parses trees of integers" $ do
      parseTree "[0 1 2]" `shouldBe` Right (Node (ELit $ LInt 0) (Node (ELit $ LInt 1) Leaf Leaf) (Node (ELit $ LInt 2) Leaf Leaf))
    it "parses trees of booleans" $ do
      parseTree "[True True False]" `shouldBe` Right (Node (ELit $ LBool True) (Node (ELit $ LBool True) Leaf Leaf) (Node (ELit $ LBool False) Leaf Leaf))
    it "parses trees of variables" $ do
      parseTree "[v0 v1 v2]" `shouldBe` Right (Node (Var "v0") (Node (Var "v1") Leaf Leaf) (Node (Var "v2") Leaf Leaf))

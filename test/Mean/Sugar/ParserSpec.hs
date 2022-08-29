{-# LANGUAGE OverloadedStrings #-}

module Mean.Sugar.ParserSpec where

import Mean.Sugar.Parser
import Mean.Sugar.Syntax
import Mean.Sugar.Viz
import Mean.Core.Syntax
import Mean.Common.Parser (parse)

import Test.Hspec

spec :: Spec
spec = do
  let fn x b = Lam (Binder (mkVar x) TyNil) b
  let idFn x = fn x (mkEVar x)
  let fOfX = App (mkEVar "f") (mkEVar "x")

  describe "pTree" $ do
    it "parses trees of integers" $ do
      parse pTree "[0 [1] [2]]" `shouldBe` Right (Node (ELit $ LInt 0) (Node (ELit $ LInt 1) Leaf Leaf) (Node (ELit $ LInt 2) Leaf Leaf))
    it "parses trees of booleans" $ do
      parse pTree "[True [True] [False]]" `shouldBe` Right (Node (ELit $ LBool True) (Node (ELit $ LBool True) Leaf Leaf) (Node (ELit $ LBool False) Leaf Leaf))
    it "parses trees of variables" $ do
      parse pTree "[v0 [v1] [v2]]" `shouldBe` Right (Node (mkEVar "v0") (Node (mkEVar "v1") Leaf Leaf) (Node (mkEVar "v2") Leaf Leaf))
    it "parses trees of lambdas" $ do
      parse pTree "[\\x.x [\\x.x] [\\x.x]]" `shouldBe` Right (Node (idFn "x") (Node (idFn "x") Leaf Leaf) (Node (idFn "x") Leaf Leaf))
    it "parses trees of function applications" $ do
      parse pTree "[(f x) [f x] [f(x)]]" `shouldBe` Right (Node fOfX (Node fOfX Leaf Leaf) (Node fOfX Leaf Leaf))

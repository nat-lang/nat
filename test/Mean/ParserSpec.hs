{-# LANGUAGE OverloadedStrings #-}

module Mean.ParserSpec where

import Control.Exception (evaluate)
import Mean.Parser
import Mean.Syntax
import Mean.Typing
import Mean.Viz
import Test.Hspec

spec :: Spec
spec = do
  describe "Mean.Parser.pType" $ do
    it "parses primitive types" $ do
      parse pType "<n>" `shouldBe` Right tyInt
      parse pType "<t>" `shouldBe` Right tyBool
    it "parses variable types" $ do
      parse pType "<A>" `shouldBe` Right (TyVar $ TV "A")
      parse pType "<ZZ>" `shouldBe` Right (TyVar $ TV "ZZ")
    it "parses functional types" $ do
      parse pType "<n,t>" `shouldBe` Right (TyFun tyInt tyBool)
      parse pType "<n,<A,t>>" `shouldBe` Right (TyFun tyInt (TyFun (TyVar $ TV "A") tyBool))

  describe "mean.Parser.pExpr" $ do
    it "parses literal expressions" $ do
      parse pExpr "1" `shouldBe` Right (ELit $ LInt 1)
      parse pExpr "0" `shouldBe` Right (ELit $ LInt 0)
      parse pExpr "True" `shouldBe` Right (ELit $ LBool True)
      parse pExpr "False" `shouldBe` Right (ELit $ LBool False)

    it "parses variables" $ do
      parse pExpr "a" `shouldBe` Right (Var "a")

    it "parses untyped lambdas" $ do
      parse pExpr "\\a.a" `shouldBe` Right (Lam (Binder "a" TyNil) (Var "a"))

  describe "Mean.Parser.pTree" $ do
    it "parses trees of integers" $ do
      parse pTree "[0 1 2]" `shouldBe` Right (Node (ELit $ LInt 0) (Node (ELit $ LInt 1) Leaf Leaf) (Node (ELit $ LInt 2) Leaf Leaf))
    it "parses trees of booleans" $ do
      parse pTree "[True True False]" `shouldBe` Right (Node (ELit $ LBool True) (Node (ELit $ LBool True) Leaf Leaf) (Node (ELit $ LBool False) Leaf Leaf))
    it "parses trees of variables" $ do
      parse pTree "[v0 v1 v2]" `shouldBe` Right (Node (Var "v0") (Node (Var "v1") Leaf Leaf) (Node (Var "v2") Leaf Leaf))

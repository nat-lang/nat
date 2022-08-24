{-# LANGUAGE OverloadedStrings #-}

module Mean.ParserSpec where

import Mean.Parser
import Mean.Syntax
import Mean.Typing
import Mean.Viz
import Test.Hspec

spec :: Spec
spec = do
  let fn x b = Lam (Binder x TyNil) b
  let idFn x = fn x (Var x)
  let fOfX = App (Var "f") (Var "x")

  describe "pType" $ do
    it "parses the primitive types" $ do
      parse pType "<n>" `shouldBe` Right tyInt
      parse pType "<t>" `shouldBe` Right tyBool
    it "parses variable types" $ do
      parse pType "<A>" `shouldBe` Right (TyVar $ TV "A")
      parse pType "<ZZ>" `shouldBe` Right (TyVar $ TV "ZZ")
    it "parses functional types" $ do
      parse pType "<n,t>" `shouldBe` Right (TyFun tyInt tyBool)
      parse pType "<n,<A,t>>" `shouldBe` Right (TyFun tyInt (TyFun (TyVar $ TV "A") tyBool))

  describe "pInt" $ do
    it "parses literal integers" $ do
      parse pExpr "1" `shouldBe` Right (ELit $ LInt 1)
      parse pExpr "0" `shouldBe` Right (ELit $ LInt 0)

  describe "pBool" $ do
    it "parses literal booleans" $ do
      parse pExpr "True" `shouldBe` Right (ELit $ LBool True)
      parse pExpr "False" `shouldBe` Right (ELit $ LBool False)

  describe "pVar" $ do
    it "parses alphanumeric variables" $ do
      parse pExpr "a" `shouldBe` Right (Var "a")
      parse pExpr "z1" `shouldBe` Right (Var "z1")

  describe "pLam" $ do
    it "parses untyped lambdas" $ do
      parse pLam "\\a.a" `shouldBe` Right (idFn "a")
    it "parses typed lambdas" $ do
      parse pLam "\\a:<A>.a" `shouldBe` Right (Lam (Binder "a" (TyVar $ TV "A")) (Var "a"))
      parse pLam "\\a:<n,t>.a" `shouldBe` Right (Lam (Binder "a" (TyFun tyInt tyBool)) (Var "a"))
    it "parses lambdas with complex bodies" $ do
      parse pLam "\\f.(\\x.f(x x))(\\x.f(x x))"
        `shouldBe` Right
          ( fn
              "f"
              ( App
                  (fn "x" (App (Var "f") (App (Var "x") (Var "x"))))
                  (fn "x" (App (Var "f") (App (Var "x") (Var "x"))))
              )
          )

  describe "pExpr" $ do
    it "parses functional application of variables" $ do
      parse pExpr "f x" `shouldBe` Right fOfX
      parse pExpr "f(x)" `shouldBe` Right fOfX
    it "parses functional application of lambdas" $ do
      parse pExpr "(\\f.f)\\f.f" `shouldBe` Right (App (idFn "f") (idFn "f"))
      parse pExpr "(\\f.f)(\\f.f)" `shouldBe` Right (App (idFn "f") (idFn "f"))
    it "parses functional application of function applications" $ do
      parse pExpr "f x (f x)" `shouldBe` Right (App fOfX fOfX)
      parse pExpr "f(x)(f x)" `shouldBe` Right (App fOfX fOfX)

  describe "pTree" $ do
    it "parses trees of integers" $ do
      parse pTree "[0 [1] [2]]" `shouldBe` Right (Node (ELit $ LInt 0) (Node (ELit $ LInt 1) Leaf Leaf) (Node (ELit $ LInt 2) Leaf Leaf))
    it "parses trees of booleans" $ do
      parse pTree "[True [True] [False]]" `shouldBe` Right (Node (ELit $ LBool True) (Node (ELit $ LBool True) Leaf Leaf) (Node (ELit $ LBool False) Leaf Leaf))
    it "parses trees of variables" $ do
      parse pTree "[v0 [v1] [v2]]" `shouldBe` Right (Node (Var "v0") (Node (Var "v1") Leaf Leaf) (Node (Var "v2") Leaf Leaf))
    it "parses trees of lambdas" $ do
      parse pTree "[\\x.x [\\x.x] [\\x.x]]" `shouldBe` Right (Node (idFn "x") (Node (idFn "x") Leaf Leaf) (Node (idFn "x") Leaf Leaf))
    it "parses trees of function applications" $ do
      parse pTree "[(f x) [f x] [f(x)]]" `shouldBe` Right (Node fOfX (Node fOfX Leaf Leaf) (Node fOfX Leaf Leaf))
{-# LANGUAGE OverloadedStrings #-}

module Mean.Core.ParserSpec where

import Mean.Common.Parser (parse)
import Mean.Core.Factory (fOfX, fn, idFn)
import Mean.Core.Parser
import Mean.Core.Syntax
import Mean.Core.Type
import Mean.Core.Viz
import Test.Hspec

a = mkVar "a"

z1 = mkVar "z1"

one = LInt 1

zero = LInt 0

true = LBool True

false = LBool False

spec :: Spec
spec = do
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
      parse pInt "1" `shouldBe` Right one
      parse pInt "0" `shouldBe` Right zero

  describe "pBool" $ do
    it "parses literal booleans" $ do
      parse pBool "True" `shouldBe` Right true
      parse pBool "False" `shouldBe` Right false

  describe "pVar" $ do
    it "parses alphanumeric variables" $ do
      parse pVar "a" `shouldBe` Right a
      parse pVar "z1" `shouldBe` Right z1

  describe "pCLit" $ do
    it "parses literals" $ do
      parse pCLit "1" `shouldBe` Right (CLit one)
      parse pCLit "0" `shouldBe` Right (CLit zero)
      parse pCLit "True" `shouldBe` Right (CLit true)
      parse pCLit "False" `shouldBe` Right (CLit false)

  describe "pCVar" $ do
    it "parses alphanumeric variables" $ do
      parse pCVar "a" `shouldBe` Right (CVar a)
      parse pCVar "z1" `shouldBe` Right (CVar z1)

  describe "pCLam" $ do
    it "parses untyped lambdas" $ do
      parse pCLam "\\a.a" `shouldBe` Right (idFn "a")
    it "parses typed lambdas" $ do
      parse pCLam "\\a:<A>.a" `shouldBe` Right (mkCLam (Binder (mkVar "a") (TyVar $ TV "A")) (mkCVar "a"))
      parse pCLam "\\a:<n,t>.a" `shouldBe` Right (mkCLam (Binder (mkVar "a") (TyFun tyInt tyBool)) (mkCVar "a"))
    it "parses lambdas with complex bodies" $ do
      parse pCLam "\\f.(\\x.f(x x))(\\x.f(x x))"
        `shouldBe` Right
          ( fn
              "f"
              ( mkCApp
                  (fn "x" (mkCApp (mkCVar "f") (mkCApp (mkCVar "x") (mkCVar "x"))))
                  (fn "x" (mkCApp (mkCVar "f") (mkCApp (mkCVar "x") (mkCVar "x"))))
              )
          )

  describe "pCExpr" $ do
    it "parses functional application of variables" $ do
      parse pCExpr "f x" `shouldBe` Right fOfX
      parse pCExpr "f(x)" `shouldBe` Right fOfX
    it "parses functional application of lambdas" $ do
      parse pCExpr "(\\f.f)\\f.f" `shouldBe` Right (mkCApp (idFn "f") (idFn "f"))
      parse pCExpr "(\\f.f)(\\f.f)" `shouldBe` Right (mkCApp (idFn "f") (idFn "f"))
    it "parses functional application of function applications" $ do
      parse pCExpr "f x (f x)" `shouldBe` Right (mkCApp fOfX fOfX)
      parse pCExpr "f(x)(f x)" `shouldBe` Right (mkCApp fOfX fOfX)

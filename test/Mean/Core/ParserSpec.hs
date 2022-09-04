{-# LANGUAGE OverloadedStrings #-}

module Mean.Core.ParserSpec where

import Mean.Common.Parser (parse)
import Mean.Core.Encoding
import Mean.Core.Parser
import Mean.Core.Syntax
import Mean.Core.Type
import Mean.Core.Viz
import Test.Hspec
import Prelude hiding ((*))

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
      parse pInt "1" `shouldBe` Right lOne
      parse pInt "0" `shouldBe` Right lZero

  describe "pBool" $ do
    it "parses literal booleans" $ do
      parse pBool "True" `shouldBe` Right lTrue
      parse pBool "False" `shouldBe` Right lFalse

  describe "pVar" $ do
    it "parses alphanumeric variables" $ do
      parse pVar "a" `shouldBe` Right (mkVar "a")
      parse pVar "z1" `shouldBe` Right (mkVar "z1")

  describe "pCLit" $ do
    it "parses litera expressions" $ do
      parse pCLit "1" `shouldBe` Right (CLit lOne)
      parse pCLit "0" `shouldBe` Right (CLit lZero)
      parse pCLit "True" `shouldBe` Right (CLit lTrue)
      parse pCLit "False" `shouldBe` Right (CLit lFalse)

  describe "pCVar" $ do
    it "parses alphanumeric variable expressions" $ do
      parse pCVar "a" `shouldBe` Right (mkCVar "a")
      parse pCVar "z1" `shouldBe` Right (mkCVar "z1")

  describe "pCLam" $ do
    it "parses untyped lambdas" $ do
      parse pCLam "\\x.x" `shouldBe` Right (x ~> x)
    it "parses typed lambdas" $ do
      parse pCLam "\\a:<A>.a" `shouldBe` Right (mkCLam (Binder (mkVar "a") (TyVar $ TV "A")) (mkCVar "a"))
      parse pCLam "\\a:<n,t>.a" `shouldBe` Right (mkCLam (Binder (mkVar "a") (TyFun tyInt tyBool)) (mkCVar "a"))
    it "parses lambdas with complex bodies" $ do
      parse pCLam "\\f.(\\x.f(x x))(\\x.f(x x))"
        `shouldBe` Right (f ~> (x ~> (f * (x * x))) * (x ~> (f * (x * x))))

  describe "pCExpr" $ do
    it "parses functional application of variables" $ do
      parse pCExpr "f x" `shouldBe` Right (f * x)
      parse pCExpr "f(x)" `shouldBe` Right (f * x)
    it "parses functional application of lambdas" $ do
      parse pCExpr "(\\f.f)\\f.f" `shouldBe` Right ((f ~> f) * (f ~> f))
      parse pCExpr "(\\f.f)(\\f.f)" `shouldBe` Right ((f ~> f) * (f ~> f))
    it "parses functional application of function applications" $ do
      parse pCExpr "f x (f x)" `shouldBe` Right (f * x * (f * x))
      parse pCExpr "f(x)(f x)" `shouldBe` Right (f * x * (f * x))

{-# LANGUAGE OverloadedStrings #-}

module Mean.Module.ParserSpec where  

import Mean.Parser (parse)
import Mean.Module
import Mean.Syntax
import Test.Hspec

spec :: Spec
spec = do
  describe "pMDecl" $ do
    it "parses let declarations" $ do
      parse pMDecl "let foo = bar" `shouldBe` Right (MDecl "foo" (mkEVar "bar"))

  describe "pModule" $ do
    it "parses modules" $ do
      parse pModule "let foo = bar \n let bar = foo" `shouldBe` Right [MDecl "foo" (mkEVar "bar"), MDecl "bar" (mkEVar "foo")]
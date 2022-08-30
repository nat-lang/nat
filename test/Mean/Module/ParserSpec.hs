{-# LANGUAGE OverloadedStrings #-}

module Mean.Module.ParserSpec where  
  
import Mean.Module.Parser
import Mean.Common.Parser (parse)
import Mean.Module.Syntax
import Mean.Sugar.Syntax
import Mean.Module.Viz
import Test.Hspec

spec :: Spec
spec = do
  describe "pMDecl" $ do
    it "parses let declarations" $ do
      parse pMDecl "let foo = bar" `shouldBe` Right (MDecl "foo" (mkSVar "bar"))

  describe "pModule" $ do
    it "parses modules" $ do
      parse pModule "let foo = bar \n let bar = foo" `shouldBe` Right [MDecl "foo" (mkSVar "bar"), MDecl "bar" (mkSVar "foo")]
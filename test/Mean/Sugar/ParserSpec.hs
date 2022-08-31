{-# LANGUAGE OverloadedStrings #-}

module Mean.Sugar.ParserSpec where

import Mean.Common.Parser (parse)
import Mean.Core.Encoding
import Mean.Core.Syntax
import Mean.Sugar.Parser
import Mean.Sugar.Syntax
import Mean.Sugar.Viz
import qualified Mean.Sugar.Encoding as SEnc
import Test.Hspec
import Prelude hiding (id)

spec :: Spec
spec = do
  describe "pTree" $ do
    it "parses trees of integers" $ do
      parse pTree "[0 [1] [2]]"
        `shouldBe` Right (Node (CLit $ LInt 0) (Node (CLit $ LInt 1) Leaf Leaf) (Node (CLit $ LInt 2) Leaf Leaf))
    it "parses trees of booleans" $ do
      parse pTree "[True [True] [False]]"
        `shouldBe` Right (Node (CLit $ LBool True) (Node (CLit $ LBool True) Leaf Leaf) (Node (CLit $ LBool False) Leaf Leaf))
    it "parses trees of variables" $ do
      parse pTree "[v0 [v1] [v2]]"
        `shouldBe` Right (Node (mkCVar "v0") (Node (mkCVar "v1") Leaf Leaf) (Node (mkCVar "v2") Leaf Leaf))
    it "parses trees of binders" $ do
      parse pTree "[\\x [\\x] [\\x]]"
        `shouldBe` Right (Node (cBind "x") (Node (cBind "x") Leaf Leaf) (Node (cBind "x") Leaf Leaf))
    it "parses trees of lambdas" $ do
      parse pTree "[\\x.x [\\x.x] [\\x.x]]"
        `shouldBe` Right (Node id (Node id Leaf Leaf) (Node id Leaf Leaf))
    it "parses trees of function applications" $ do
      parse pTree "[(f x) [f x] [f(x)]]"
        `shouldBe` Right (Node fOfX (Node fOfX Leaf Leaf) (Node fOfX Leaf Leaf))

  describe "pSExpr" $ do
    let tree = STree $ Node (cBind "y") (Node (fn "x" (mkCApp (mkCVar "x") (mkCVar "y"))) Leaf Leaf) (Node id Leaf Leaf)

    it "parses applications of lambdas to trees" $ do
      parse pSExpr "(\\x.x)([\\y [\\x.x(y)] [\\x.x]])" `shouldBe` Right (mkSApp SEnc.id tree)

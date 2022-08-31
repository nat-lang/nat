{-# LANGUAGE OverloadedStrings #-}

module Mean.Sugar.ParserSpec where

import Mean.Common.Parser (parse)
import Mean.Core.Encoding
import Mean.Core.Syntax
import Mean.Sugar.Parser
import Mean.Sugar.Syntax
import Mean.Sugar.Viz
import Test.Hspec
import Prelude hiding (id, (*))

spec :: Spec
spec = do
  describe "pTree" $ do
    it "parses trees of integers" $ do
      parse pTree "[0 [1] [0]]"
        `shouldBe` Right (Node (CLit $ LInt 0) (Node (CLit lOne) Leaf Leaf) (Node (CLit lZero) Leaf Leaf))
    it "parses trees of booleans" $ do
      parse pTree "[True [True] [False]]"
        `shouldBe` Right (Node (CLit $ LBool True) (Node (CLit lTrue) Leaf Leaf) (Node (CLit lFalse) Leaf Leaf))
    it "parses trees of variables" $ do
      parse pTree "[x [y] [z]]"
        `shouldBe` Right (Node x (Node y Leaf Leaf) (Node z Leaf Leaf))
    it "parses trees of binders" $ do
      let bX = mkCBind x
      parse pTree "[\\x [\\x] [\\x]]"
        `shouldBe` Right (Node bX (Node bX Leaf Leaf) (Node bX Leaf Leaf))
    it "parses trees of lambdas" $ do
      parse pTree "[\\x.x [\\x.x] [\\x.x]]"
        `shouldBe` Right (Node id (Node id Leaf Leaf) (Node id Leaf Leaf))
    it "parses trees of function applications" $ do
      parse pTree "[(f x) [f x] [f(x)]]"
        `shouldBe` Right (Node (f * x) (Node (f * x) Leaf Leaf) (Node (f * x) Leaf Leaf))

  describe "pSExpr" $ do
    let tree = STree $ Node (mkCBind y) (Node (x ~> (x * y)) Leaf Leaf) (Node (x ~> x) Leaf Leaf)

    it "parses applications of lambdas to trees" $ do
      let sId = mkSLam (mkBinder x) (mkSVar "x")
      parse pSExpr "(\\x.x)([\\y [\\x.x(y)] [\\x.x]])" `shouldBe` Right (mkSApp sId tree)

{-# LANGUAGE OverloadedStrings #-}

module Mean.Sugar.ParserSpec where

import Mean.Common.Parser (parse)
import Mean.Core.Encoding (lFalse, lOne, lTrue, lZero)
import Mean.Core.Syntax hiding ((*), (~>))
import Mean.Sugar.Encoding
import Mean.Sugar.Parser
import Mean.Sugar.Syntax
import Mean.Sugar.Viz
import Test.Hspec
import Prelude hiding (id, (*))

spec :: Spec
spec = do
  describe "pSTree" $ do
    it "parses trees of integers" $ do
      parse pSTree "[0 [1] [0]]"
        `shouldBe` Right (STree (Node (SLit $ LInt 0) (Node (SLit lOne) Leaf Leaf) (Node (SLit lZero) Leaf Leaf)))
    it "parses trees of booleans" $ do
      parse pSTree "[True [True] [False]]"
        `shouldBe` Right (STree (Node (SLit $ LBool True) (Node (SLit lTrue) Leaf Leaf) (Node (SLit lFalse) Leaf Leaf)))
    it "parses trees of variables" $ do
      parse pSTree "[x [y] [z]]"
        `shouldBe` Right (STree (Node x (Node y Leaf Leaf) (Node z Leaf Leaf)))
    it "parses trees of binders" $ do
      let bX = mkSBind x
      parse pSTree "[\\x [\\x] [\\x]]"
        `shouldBe` Right (STree (Node bX (Node bX Leaf Leaf) (Node bX Leaf Leaf)))
    it "parses trees of lambdas" $ do
      parse pSTree "[\\x.x [\\x.x] [\\x.x]]"
        `shouldBe` Right (STree (Node (x ~> x) (Node (x ~> x) Leaf Leaf) (Node (x ~> x) Leaf Leaf)))
    it "parses trees of function applications" $ do
      parse pSTree "[(f x) [f x] [f(x)]]"
        `shouldBe` Right (STree (Node (f * x) (Node (f * x) Leaf Leaf) (Node (f * x) Leaf Leaf)))

  describe "pSExpr" $ do
    let tree = STree $ Node (mkSBind y) (Node (x ~> (x * y)) Leaf Leaf) (Node (x ~> x) Leaf Leaf)

    it "parses applications of lambdas to trees" $ do
      parse pSExpr "(\\x.x)([\\y [\\x.x(y)] [\\x.x]])" `shouldBe` Right ((x ~> x) * tree)

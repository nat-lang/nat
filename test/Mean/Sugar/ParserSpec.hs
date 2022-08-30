{-# LANGUAGE OverloadedStrings #-}

module Mean.Sugar.ParserSpec where

import Mean.Common.Parser (parse)
import qualified Mean.Core.Factory as F
import Mean.Core.Syntax
import qualified Mean.Sugar.Factory as SugarFactory
import Mean.Sugar.Parser
import Mean.Sugar.Syntax
import Mean.Sugar.Viz
import Test.Hspec

bind x = BNode $ Binder (mkVar x) TyNil

idFn = ENode . F.idFn

fOfX = ENode F.fOfX

v = ENode . mkCVar

lit = ENode . CLit

spec :: Spec
spec = do
  describe "pTree" $ do
    it "parses trees of integers" $ do
      parse pTree "[0 [1] [2]]"
        `shouldBe` Right (Node (lit $ LInt 0) (Node (lit $ LInt 1) Leaf Leaf) (Node (lit $ LInt 2) Leaf Leaf))
    it "parses trees of booleans" $ do
      parse pTree "[True [True] [False]]"
        `shouldBe` Right (Node (lit $ LBool True) (Node (lit $ LBool True) Leaf Leaf) (Node (lit $ LBool False) Leaf Leaf))
    it "parses trees of variables" $ do
      parse pTree "[v0 [v1] [v2]]"
        `shouldBe` Right (Node (v "v0") (Node (v "v1") Leaf Leaf) (Node (v "v2") Leaf Leaf))
    it "parses trees of binders" $ do
      parse pTree "[\\x [\\x] [\\x]]"
        `shouldBe` Right (Node (bind "x") (Node (bind "x") Leaf Leaf) (Node (bind "x") Leaf Leaf))
    it "parses trees of lambdas" $ do
      parse pTree "[\\x.x [\\x.x] [\\x.x]]"
        `shouldBe` Right (Node (idFn "x") (Node (idFn "x") Leaf Leaf) (Node (idFn "x") Leaf Leaf))
    it "parses trees of function applications" $ do
      parse pTree "[(f x) [f x] [f(x)]]"
        `shouldBe` Right (Node fOfX (Node fOfX Leaf Leaf) (Node fOfX Leaf Leaf))

  describe "pSExpr" $ do
    let tree = STree $ Node (bind "y") (Node (ENode $ F.fn "x" (mkCApp (mkCVar "x") (mkCVar "y"))) Leaf Leaf) (Node (ENode F.id) Leaf Leaf)

    it "parses applications of lambdas to trees" $ do
      parse pSExpr "(\\x.x)([\\y [\\x.x(y)] [\\x.x]])" `shouldBe` Right (mkSApp SugarFactory.id tree)

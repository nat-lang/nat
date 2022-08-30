{-# LANGUAGE OverloadedStrings #-}

module Mean.Sugar.ParserSpec where

import Mean.Sugar.Parser
import Mean.Sugar.Syntax
import Mean.Sugar.Viz
import Mean.Core.Syntax
import Mean.Common.Parser (parse)

import Test.Hspec

n c = case c of
  CoreExpr {} -> ENode c
  Binder {} -> BNode c

bind x = BNode $ Binder (mkVar x) TyNil
fn x = mkCLam (bind x)
idFn x = fn x (mkCVar x)
fOfX = mkCApp (mkCVar "f") (mkCVar "x")
lit l = ENode $ case l of
  LInt {} -> CLit l
  LBool {} -> CBool l

spec :: Spec
spec = do
  describe "pTree" $ do
    it "parses trees of integers" $ do
      parse pTree "[0 [1] [2]]" `shouldBe`
        Right (Node (CLit $ LInt 0) (Node (CLit $ LInt 1) Leaf Leaf) (Node (CLit $ LInt 2) Leaf Leaf))
    it "parses trees of booleans" $ do
      parse pTree "[True [True] [False]]" `shouldBe`
        Right (Node (CLit $ LBool True) (Node (CLit $ LBool True) Leaf Leaf) (Node (CLit $ LBool False) Leaf Leaf))
    it "parses trees of variables" $ do
      parse pTree "[v0 [v1] [v2]]" `shouldBe`
        Right (Node (mkSVar "v0") (Node (mkSVar "v1") Leaf Leaf) (Node (mkSVar "v2") Leaf Leaf))
    it "parses trees of binders" $ do
      parse pTree "[\\x [\\x] [\\x]]" `shouldBe`
        Right (Node (bind "x") (Node (bind "x") Leaf Leaf) (Node (bind "x") Leaf Leaf))
    it "parses trees of lambdas" $ do
      parse pTree "[\\x.x [\\x.x] [\\x.x]]" `shouldBe`
        Right (Node (idFn "x") (Node (idFn "x") Leaf Leaf) (Node (idFn "x") Leaf Leaf))
    it "parses trees of function applications" $ do
      parse pTree "[(f x) [f x] [f(x)]]" `shouldBe`
        Right (Node fOfX (Node fOfX Leaf Leaf) (Node fOfX Leaf Leaf))

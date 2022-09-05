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
import Prelude hiding (id, (&&), (*), (>), (||))

spec :: Spec
spec = do
  describe "pSTree" $ do
    it "parses trees of integers" $ do
      parse pSTree "[0 [1] [0]]"
        `shouldBe` Right (STree (Node (SLit $ LInt 0) (Node (SLit lOne) Leaf Leaf) (Node (SLit lZero) Leaf Leaf)))
    it "parses trees of booleans" $ do
      parse pSTree "[True [True] [False]]"
        `shouldBe` Right (STree (Node (SLit $ LBool True) (Node true Leaf Leaf) (Node false Leaf Leaf)))
    it "parses trees of variables" $ do
      parse pSTree "[x [y] [z]]"
        `shouldBe` Right (STree (Node x (Node y Leaf Leaf) (Node z Leaf Leaf)))
    it "parses trees of binders" $ do
      parse pSTree "[\\x [\\y] [\\z]]"
        `shouldBe` Right (STree (Node (mkSBind x) (Node (mkSBind y) Leaf Leaf) (Node (mkSBind z) Leaf Leaf)))
    it "parses trees of unary operations" $ do
      parse pSTree "[!x [!(\\y.z)] [!(y z)]]"
        `shouldBe` Right (STree (Node (not' x) (Node (not' (y ~> z)) Leaf Leaf) (Node (not' (y * z)) Leaf Leaf)))
    it "parses trees of binary operations" $ do
      parse pSTree "[x || x [(f y) != (f x)] [(\\f.x) && (\\f.y)]]"
        `shouldBe` Right (STree (Node (x || x) (Node ((f * y) !== (f * x)) Leaf Leaf) (Node ((f ~> x) && (f ~> y)) Leaf Leaf)))
    it "parses trees of lambdas" $ do
      parse pSTree "[\\x.x [\\y.y] [\\z.z]]"
        `shouldBe` Right (STree (Node (x ~> x) (Node (y ~> y) Leaf Leaf) (Node (z ~> z) Leaf Leaf)))
    it "parses trees of ternary conditionals" $ do
      parse pSTree "[if z then y else x [\\x.if x then z else y] [if y then x else z]]"
        `shouldBe` Right (STree (Node (z ? y > x) (Node (x ~> x ? z > y) Leaf Leaf) (Node (y ? x > z) Leaf Leaf)))
    it "parses trees of sets" $ do
      parse pSTree "[{0,1} [{True,False}] [{x,y,z}]]"
        `shouldBe` Right (STree (Node (SSet [SLit lZero, SLit lOne]) (Node (SSet [true, false]) Leaf Leaf) (Node (SSet [x, y, z]) Leaf Leaf)))
    it "parses trees of function applications" $ do
      parse pSTree "[(f x) [f x] [f(x)]]"
        `shouldBe` Right (STree (Node (f * x) (Node (f * x) Leaf Leaf) (Node (f * x) Leaf Leaf)))

  describe "pSCase" $ do
    it "parses case statements" $ do
      parse pSCase "case x of\n\tTrue: y\n\tFalse: z" `shouldBe` Right (SCase x [(true, y), (false, z)])
    it "parses nested case statements" $ do
      parse pSCase "case x of\n\tTrue: y\n\tFalse: case z of\n\t\tFalse: y\n\t\tTrue: x" `shouldBe` Right (SCase x [(true, y), (false, SCase z [(false, y), (true, x)])])

  describe "pSSet" $ do
    it "parses sets of integers" $ do
      parse pSSet "{0, 1}" `shouldBe` Right (SSet [SLit lZero, SLit lOne])
    it "parses sets of booleans" $ do
      parse pSSet "{True, False}" `shouldBe` Right (SSet [true, false])
    it "parses sets of variables" $ do
      parse pSSet "{x,y,z}" `shouldBe` Right (SSet [x, y, z])
    it "parses sets of unary operations" $ do
      parse pSSet "{!x,!y,!z}" `shouldBe` Right (SSet [not' x, not' y, not' z])
    it "parses sets of binary operations" $ do
      parse pSSet "{x && y, y == x, z || y}" `shouldBe` Right (SSet [x && y, y === x, z || y])
    it "parses sets of lambdas" $ do
      parse pSSet "{\\x.x,\\y.y,\\z.z}" `shouldBe` Right (SSet [x ~> x, y ~> y, z ~> z])
    it "parses sets of function applications" $ do
      parse pSSet "{f(x), f y, (f z)}" `shouldBe` Right (SSet [f * x, f * y, f * z])
    it "parses sets of ternary conditionals" $ do
      parse pSSet "{if x then y else z, if y then z else x, if z then x else y}"
        `shouldBe` Right (SSet [x ? y > z, y ? z > x, z ? x > y])
    it "parses sets of trees" $ do
      parse pSSet "{[x [y][z]], [y [z][x]]}"
        `shouldBe` Right (SSet [STree (Node x (Node y Leaf Leaf) (Node z Leaf Leaf)), STree (Node y (Node z Leaf Leaf) (Node x Leaf Leaf))])

  describe "pSExpr" $ do
    let tree = STree $ Node (mkSBind y) (Node (x ~> (x * y)) Leaf Leaf) (Node (x ~> x) Leaf Leaf)

    it "parses applications of lambdas to trees" $ do
      parse pSExpr "(\\x.x)([\\y [\\x.x(y)] [\\x.x]])" `shouldBe` Right ((x ~> x) * tree)

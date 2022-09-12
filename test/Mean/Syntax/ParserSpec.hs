{-# LANGUAGE OverloadedStrings #-}

module Mean.Syntax.ParserSpec where

import Mean.Parser (parse)
import Mean.Core (lFalse, lOne, lTrue, lZero, Lit(..))
import Mean.Syntax
import Mean.Tree
import Mean.Case
import Mean.Set
import Test.Hspec
import Prelude hiding (id, (&&), (*), (>), (||))

spec :: Spec
spec = do
  let mkSet = ESet . Set

  describe "pETree" $ do
    it "parses trees of integers" $ do
      parse pETree "[0 [1] [0]]"
        `shouldBe` Right (ETree (Node (ELit $ LInt 0) (Node (ELit lOne) Leaf Leaf) (Node (ELit lZero) Leaf Leaf)))
    it "parses trees of booleans" $ do
      parse pETree "[True [True] [False]]"
        `shouldBe` Right (ETree (Node (ELit $ LBool True) (Node true Leaf Leaf) (Node false Leaf Leaf)))
    it "parses trees of variables" $ do
      parse pETree "[x [y] [z]]"
        `shouldBe` Right (ETree (Node x (Node y Leaf Leaf) (Node z Leaf Leaf)))
    it "parses trees of binders" $ do
      parse pETree "[\\x [\\y] [\\z]]"
        `shouldBe` Right (ETree (Node (mkEBind x) (Node (mkEBind y) Leaf Leaf) (Node (mkEBind z) Leaf Leaf)))
    it "parses trees of unary operations" $ do
      parse pETree "[!x [!(\\y.z)] [!(y z)]]"
        `shouldBe` Right (ETree (Node (not' x) (Node (not' (y ~> z)) Leaf Leaf) (Node (not' (y * z)) Leaf Leaf)))
    it "parses trees of binary operations" $ do
      parse pETree "[x || x [(f y) != (f x)] [(\\f.x) && (\\f.y)]]"
        `shouldBe` Right (ETree (Node (x || x) (Node ((f * y) !== (f * x)) Leaf Leaf) (Node ((f ~> x) && (f ~> y)) Leaf Leaf)))
    it "parses trees of lambdas" $ do
      parse pETree "[\\x.x [\\y.y] [\\z.z]]"
        `shouldBe` Right (ETree (Node (x ~> x) (Node (y ~> y) Leaf Leaf) (Node (z ~> z) Leaf Leaf)))
    it "parses trees of ternary conditionals" $ do
      parse pETree "[if z then y else x [\\x.if x then z else y] [if y then x else z]]"
        `shouldBe` Right (ETree (Node (z ? y > x) (Node (x ~> x ? z > y) Leaf Leaf) (Node (y ? x > z) Leaf Leaf)))
    it "parses trees of sets" $ do
      parse pETree "[{0,1} [{True,False}] [{x,y,z}]]"
        `shouldBe` Right (ETree (Node (mkSet [ELit lZero, ELit lOne]) (Node (mkSet [true, false]) Leaf Leaf) (Node (mkSet [x, y, z]) Leaf Leaf)))
    it "parses trees of function applications" $ do
      parse pETree "[(f x) [f x] [f(x)]]"
        `shouldBe` Right (ETree (Node (f * x) (Node (f * x) Leaf Leaf) (Node (f * x) Leaf Leaf)))

  describe "pECase" $ do
    let mkCase e cs = ECase (Case e cs)

    it "parses case statements" $ do
      parse pECase "case x of\n\tTrue: y\n\tFalse: z" `shouldBe` Right (mkCase x [(true, y), (false, z)])
    it "parses nested case statements" $ do
      parse pECase "case x of\n\tTrue: y\n\tFalse: case z of\n\t\tFalse: y\n\t\tTrue: x" `shouldBe` Right (mkCase x [(true, y), (false, mkCase z [(false, y), (true, x)])])

  describe "pESet" $ do
    it "parses sets of integers" $ do
      parse pESet "{0, 1}" `shouldBe` Right (mkSet [ELit lZero, ELit lOne])
    it "parses sets of booleans" $ do
      parse pESet "{True, False}" `shouldBe` Right (mkSet [true, false])
    it "parses sets of variables" $ do
      parse pESet "{x,y,z}" `shouldBe` Right (mkSet [x, y, z])
    it "parses sets of unary operations" $ do
      parse pESet "{!x,!y,!z}" `shouldBe` Right (mkSet [not' x, not' y, not' z])
    it "parses sets of binary operations" $ do
      parse pESet "{x && y, y == x, z || y}" `shouldBe` Right (mkSet [x && y, y === x, z || y])
    it "parses sets of lambdas" $ do
      parse pESet "{\\x.x,\\y.y,\\z.z}" `shouldBe` Right (mkSet [x ~> x, y ~> y, z ~> z])
    it "parses sets of function applications" $ do
      parse pESet "{f(x), f y, (f z)}" `shouldBe` Right (mkSet [f * x, f * y, f * z])
    it "parses sets of ternary conditionals" $ do
      parse pESet "{if x then y else z, if y then z else x, if z then x else y}"
        `shouldBe` Right (mkSet [x ? y > z, y ? z > x, z ? x > y])
    it "parses sets of trees" $ do
      parse pESet "{[x [y][z]], [y [z][x]]}"
        `shouldBe` Right (mkSet [ETree (Node x (Node y Leaf Leaf) (Node z Leaf Leaf)), ETree (Node y (Node z Leaf Leaf) (Node x Leaf Leaf))])

  describe "pExpr" $ do
    let p = mkEVar "p"
    let q = mkEVar "q"

    it "parses unary operations on primitives" $ do
      parse pExpr "!True" `shouldBe` Right (not' true)
      parse pExpr "!x" `shouldBe` Right (not' x)
    it "parses unary operations on expressions" $ do
      parse pExpr "!(f x)" `shouldBe` Right (not' (f * x))
    it "parses binary operations on primitives" $ do
      parse pExpr "x == y" `shouldBe` Right (x === y)
      parse pExpr "x != y" `shouldBe` Right (x !== y)
      parse pExpr "x && y" `shouldBe` Right (x && y)
      parse pExpr "x || y" `shouldBe` Right (x || y)
    it "parses binary operations on expressions" $ do
      parse pExpr "(f x) && (f y)" `shouldBe` Right ((f * x) && (f * y))
      parse pExpr "(f x) || (f y)" `shouldBe` Right ((f * x) || (f * y))
    it "parses operations of different arity on primitives in concert" $ do
      parse pExpr "!(p == q)" `shouldBe` Right (not' (p === q))
      parse pExpr "!(p != q)" `shouldBe` Right (not' (p !== q))
      parse pExpr "!(p && q)" `shouldBe` Right (not' (p && q))
      parse pExpr "!(p || q)" `shouldBe` Right (not' (p || q))
      parse pExpr "(!p && !q)" `shouldBe` Right (not' p && not' q)
      parse pExpr "(!p || !q)" `shouldBe` Right (not' p || not' q)
    it "parses operations of different arity on expressions in concert" $ do
      parse pExpr "!((f p) == (f q))" `shouldBe` Right (not' ((f * p) === (f * q)))
      parse pExpr "(!(f p) && !(f q))" `shouldBe` Right (not' (f * p) && not' (f * q))

    it "parses applications of lambdas to trees" $ do
      let tree = ETree $ Node (mkEBind y) (Node (x ~> (x * y)) Leaf Leaf) (Node (x ~> x) Leaf Leaf)
      parse pExpr "(\\x.x)[\\y [\\x.x(y)] [\\x.x]]" `shouldBe` Right ((x ~> x) * tree)
    it "parses applications of lambdas to sets" $ do
      parse pExpr "(\\x.x){x,y,z}" `shouldBe` Right ((x ~> x) * mkSet [x, y, z])

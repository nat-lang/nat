{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Nat.Syntax.SurfaceSpec where

import qualified Data.Set as Set
import Nat.Context
import Nat.Parser (parse)
import Nat.Syntax.Surface
import Nat.Syntax.Type
import Test.Hspec
import Text.RawString.QQ
import Prelude hiding ((*), (>))

[f, x, y, z, p, q] = mkEVar <$> ["f", "x", "y", "z", "p", "q"]

lZero = LInt 0

lOne = LInt 1

lTrue = LBool True

lFalse = LBool False

true = ELit lTrue

false = ELit lFalse

mkI = ELit . LInt

mkS = ESet . Set.fromList

bind (EVar x) = EBind (Binder x TyNil)

spec :: Spec
spec = do
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

  describe "pELit" $ do
    it "parses literal expressions" $ do
      parse pELit "1" `shouldBe` Right (mkI 1)
      parse pELit "0" `shouldBe` Right (mkI 0)
      parse pELit "True" `shouldBe` Right true
      parse pELit "False" `shouldBe` Right false

  describe "pEVar" $ do
    it "parses alphanumeric variable expressions" $ do
      parse pEVar "a" `shouldBe` Right (mkEVar "a")
      parse pEVar "z1" `shouldBe` Right (mkEVar "z1")

  describe "pELam" $ do
    it "parses untyped lambdas" $ do
      parse pELam "\\x.x" `shouldBe` Right (x ~> x)
    it "parses typed lambdas" $ do
      parse pELam "\\a:<A>.a" `shouldBe` Right (ELam (Binder (mkVar "a") (mkTv "A")) (mkEVar "a"))
      parse pELam "\\a:<n,t>.a" `shouldBe` Right (ELam (Binder (mkVar "a") (TyFun tyInt tyBool)) (mkEVar "a"))
    it "parses lambdas with complex bodies" $ do
      parse pELam "\\f.(\\x.f(x x))(\\x.f(x x))"
        `shouldBe` Right (f ~> (x ~> (f * (x * x))) * (x ~> (f * (x * x))))

  describe "pECond" $ do
    it "parses ternary conditionals over variables" $ do
      parse pECond "if x then y else z" `shouldBe` Right (x ? y > z)
    it "parses ternary conditionals over lambdas" $ do
      parse pECond "if \\x.x then \\y.y else \\z.z" `shouldBe` Right ((x ~> x) ? (y ~> y) > (z ~> z))
    it "parses ternary conditionals over function applications" $ do
      parse pECond "if f(x) then f(y) else f(z)" `shouldBe` Right ((f * x) ? (f * y) > (f * z))

  describe "pETree" $ do
    let intTree = ETree (Node (mkI 0) (Node (mkI 1) Leaf Leaf) (Node (mkI 0) Leaf Leaf))
    let booTree = ETree (Node (ELit $ LBool True) (Node true Leaf Leaf) (Node false Leaf Leaf))
    let varTree = ETree (Node x (Node y Leaf Leaf) (Node z Leaf Leaf))

    it "parses trees of integers" $ do
      parse pETree "[0 [1] [0]]" `shouldBe` Right intTree

    it "parses trees of booleans" $ do
      parse pETree "[True [True] [False]]" `shouldBe` Right booTree

    it "parses trees of variables" $ do
      parse pETree "[x [y] [z]]" `shouldBe` Right varTree

    it "parses trees of binders" $ do
      parse pETree "[\\x [\\y] [\\z]]"
        `shouldBe` Right (ETree (Node (bind x) (Node (bind y) Leaf Leaf) (Node (bind z) Leaf Leaf)))

    it "parses trees of unary operations" $ do
      parse pETree "[!x [!(\\y.z)] [!(y z)]]"
        `shouldBe` Right (ETree (Node (not' x) (Node (not' (y ~> z)) Leaf Leaf) (Node (not' (y * z)) Leaf Leaf)))

    it "parses trees of binary operations" $ do
      parse pETree "[x || x [(f y) != (f x)] [(\\f.x) && (\\f.y)]]"
        `shouldBe` Right (ETree (Node (x ||| x) (Node ((f * y) !== (f * x)) Leaf Leaf) (Node ((f ~> x) &&& (f ~> y)) Leaf Leaf)))

    it "parses trees of ternary conditionals " $ do
      parse pETree "[if z then y else x [\\x.if x then z else y] [if y then x else z]]"
        `shouldBe` Right (ETree (Node (z ? y > x) (Node (x ~> x ? z > y) Leaf Leaf) (Node (y ? x > z) Leaf Leaf)))

    it "parses trees of lambdas" $ do
      parse pETree "[\\x.x [\\y.y] [\\z.z]]"
        `shouldBe` Right (ETree (Node (x ~> x) (Node (y ~> y) Leaf Leaf) (Node (z ~> z) Leaf Leaf)))

    it "parses trees of sets" $ do
      parse pETree "[{0,1} [{True,False}] [{x,y,z}]]"
        `shouldBe` Right (ETree (Node (mkS [mkI 0, mkI 1]) (Node (mkS [true, false]) Leaf Leaf) (Node (mkS [x, y, z]) Leaf Leaf)))
    it "parses trees of function applications" $ do
      parse pETree "[(f x) [f x] [f(x)]]"
        `shouldBe` Right (ETree (Node (f * x) (Node (f * x) Leaf Leaf) (Node (f * x) Leaf Leaf)))

    it "parses trees of parenthesized trees" $ do
      parse pETree "[0 [( [True [True] [False]] )] [( [x [y] [z]] )] ]"
        `shouldBe` Right (ETree (Node intTree (Node booTree Leaf Leaf) (Node varTree Leaf Leaf)))

  describe "pELitCase" $ do
    it "parses literal case expressions" $ do
      parse pELitCase "case x of True -> y | False -> z" `shouldBe` Right (ELitCase x [(true, y), (false, z)])
    it "parses nested literal case expressions" $ do
      parse pELitCase "case x of True -> y | False -> case z of False -> y | True -> x" `shouldBe` Right (ELitCase x [(true, y), (false, ELitCase z [(false, y), (true, x)])])

  describe "pETyCase" $ do
    it "parses typecase expressions with variable binders" $ do
      parse pETyCase "tycase x of y:<n> -> y | z:<t> -> z" `shouldBe` Right (ETyCase x [(Binder y tyInt, y), (Binder z tyBool, z)])
    it "parses typecase expressions with complex binders" $ do
      let tyCase = "tycase x of (p,q):(<A>, <B>) -> p(q) | (p,q,f):(<A>, <B>, <C>) -> p(q)(f)"

      let binders =
            [ Binder (ETup [p, q]) (TyTup [mkTv "A", mkTv "B"]),
              Binder (ETup [p, q, f]) (TyTup [mkTv "A", mkTv "B", mkTv "C"])
            ]
      let exprs = [p * q, p * q * f]

      parse pETyCase tyCase `shouldBe` Right (ETyCase x (zip binders exprs))

  describe "pETup" $ do
    it "parses tuples of expressions" $ do
      parse pETup "(0,1,2)" `shouldBe` Right (ETup [mkI 0, mkI 1, mkI 2])
      parse pETup "(f(x), \\x.x, {0,1,2})" `shouldBe` Right (ETup [f * x, x ~> x, mkS [mkI 0, mkI 1, mkI 2]])

  describe "pESet" $ do
    it "parses sets of integers" $ do
      parse pESet "{0, 1}" `shouldBe` Right (mkS [mkI 0, mkI 1])
    it "parses sets of booleans" $ do
      parse pESet "{True, False}" `shouldBe` Right (mkS [true, false])
    it "parses sets of variables" $ do
      parse pESet "{x,y,z}" `shouldBe` Right (mkS [x, y, z])
    it "parses sets of unary operations" $ do
      parse pESet "{!x,!y,!z}" `shouldBe` Right (mkS [not' x, not' y, not' z])
    it "parses sets of binary operations" $ do
      parse pESet "{x && y, y == x, z || y}" `shouldBe` Right (mkS [x &&& y, y === x, z ||| y])
    it "parses sets of lambdas" $ do
      parse pESet "{\\x.x,\\y.y,\\z.z}" `shouldBe` Right (mkS [x ~> x, y ~> y, z ~> z])
    it "parses sets of function applications" $ do
      parse pESet "{f(x), f y, (f z)}" `shouldBe` Right (mkS [f * x, f * y, f * z])
    it "parses sets of ternary conditionals" $ do
      parse pESet "{if x then y else z, if y then z else x, if z then x else y}"
        `shouldBe` Right (mkS [x ? y > z, y ? z > x, z ? x > y])
    it "parses sets of trees" $ do
      parse pESet "{[x [y][z]], [y [z][x]]}"
        `shouldBe` Right (mkS [ETree (Node x (Node y Leaf Leaf) (Node z Leaf Leaf)), ETree (Node y (Node z Leaf Leaf) (Node x Leaf Leaf))])

  describe "pEQnt" $ do
    let (EVar pV) = p

    it "parses quantification over variables" $ do
      parse pEQnt "forall p in q . p == p" `shouldBe` Right (univ [(pV, q)] (p === p))
      parse pEQnt "exists p in q . p == p" `shouldBe` Right (exis [(pV, q)] (p === p))
      parse pEQnt "the p in q . p == p" `shouldBe` Right (the [(pV, q)] (p === p))

    it "parses quantification over expressions" $ do
      let s = ESet $ Set.fromList [mkI 1, mkI 2, mkI 3]
      parse pEQnt "forall p in {1,2,3} . p == p" `shouldBe` Right (univ [(pV, s)] (p === p))
      parse pEQnt "exists p in {1,2,3} . p == p" `shouldBe` Right (exis [(pV, s)] (p === p))
      parse pEQnt "the p in {1,2,3} . p == p" `shouldBe` Right (the [(pV, s)] (p === p))

      parse pEQnt "forall p in f(x) . p == p" `shouldBe` Right (univ [(pV, f * x)] (p === p))
      parse pEQnt "exists p in f(x) . p == p" `shouldBe` Right (exis [(pV, f * x)] (p === p))
      parse pEQnt "the p in f(x) . p == p" `shouldBe` Right (the [(pV, f * x)] (p === p))

  describe "pExpr" $ do
    it "parses functional application of variables" $ do
      parse pExpr "f x" `shouldBe` Right (f * x)
      parse pExpr "f(x)" `shouldBe` Right (f * x)
    it "parses functional application of lambdas" $ do
      parse pExpr "(\\f.f)\\f.f" `shouldBe` Right ((f ~> f) * (f ~> f))
      parse pExpr "(\\f.f)(\\f.f)" `shouldBe` Right ((f ~> f) * (f ~> f))
    it "parses functional application of function applications" $ do
      parse pExpr "f x (f x)" `shouldBe` Right (f * x * (f * x))
      parse pExpr "f(x)(f x)" `shouldBe` Right (f * x * (f * x))

    it "parses unary operations on primitives" $ do
      parse pExpr "!True" `shouldBe` Right (not' true)
      parse pExpr "!x" `shouldBe` Right (not' x)
    it "parses unary operations on expressions" $ do
      parse pExpr "!(f x)" `shouldBe` Right (not' (f * x))
    it "parses binary operations on primitives" $ do
      parse pExpr "x == y" `shouldBe` Right (x === y)
      parse pExpr "x != y" `shouldBe` Right (x !== y)
      parse pExpr "x && y" `shouldBe` Right (x &&& y)
      parse pExpr "x || y" `shouldBe` Right (x ||| y)
      parse pExpr "x + y" `shouldBe` Right (EBinOp Add x y)
      parse pExpr "x - y" `shouldBe` Right (EBinOp Sub x y)
    it "parses binary operations on expressions" $ do
      parse pExpr "(f x) && (f y)" `shouldBe` Right ((f * x) &&& (f * y))
      parse pExpr "(f x) || (f y)" `shouldBe` Right ((f * x) ||| (f * y))
      parse pExpr "(f x) + (f y)" `shouldBe` Right (EBinOp Add (f * x) (f * y))

    it "parses operations of different arity on primitives in concert" $ do
      parse pExpr "!(p == q)" `shouldBe` Right (not' (p === q))
      parse pExpr "!(p != q)" `shouldBe` Right (not' (p !== q))
      parse pExpr "!(p && q)" `shouldBe` Right (not' (p &&& q))
      parse pExpr "!(p || q)" `shouldBe` Right (not' (p ||| q))
      parse pExpr "(!p && !q)" `shouldBe` Right (not' p &&& not' q)
      parse pExpr "(!p || !q)" `shouldBe` Right (not' p ||| not' q)
    it "parses operations of different arity on expressions in concert" $ do
      parse pExpr "!((f p) == (f q))" `shouldBe` Right (not' ((f * p) === (f * q)))
      parse pExpr "(!(f p) && !(f q))" `shouldBe` Right (not' (f * p) &&& not' (f * q))

    it "parses applications of lambdas to trees" $ do
      let tree = ETree $ Node (bind y) (Node (x ~> (x * y)) Leaf Leaf) (Node (x ~> x) Leaf Leaf)
      parse pExpr "(\\x.x)[\\y [\\x.x(y)] [\\x.x]]" `shouldBe` Right ((x ~> x) * tree)
    it "parses applications of lambdas to sets" $ do
      parse pExpr "(\\x.x){x,y,z}" `shouldBe` Right ((x ~> x) * mkS [x, y, z])

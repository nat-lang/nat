{-# LANGUAGE OverloadedStrings #-}

module Nat.Evaluation.SurfaceSpec where

import qualified Data.Set as Set
import Debug.Trace (traceM)
import Nat.Context
import qualified Nat.Context as C
import Nat.Evaluation.Context
import Nat.Evaluation.Surface hiding ((*=), (@=))
import qualified Nat.Evaluation.Surface as E
import Nat.Parser (parse)
import Nat.Reduction
import Nat.Syntax.Surface
import Nat.Syntax.Type
import Nat.Unification
import Test.HUnit ((@?=))
import Test.Hspec
import Prelude hiding (LTE, id, (*), (>))

[f, x, y, z, l, r, a, b, c, e, p, q, n, m] = mkEVar <$> ["f", "x", "y", "z", "l", "r", "a", "b", "c", "e", "p", "q", "n", "m"]

zero = ELit $ LInt 0

one = ELit $ LInt 1

true = ELit $ LBool True

false = ELit $ LBool False

id = x ~> x

mkS = ESet . Set.fromList

mkI = ELit . LInt

spec :: Spec
spec = do
  describe "sub" $ do
    let zV = mkVar "z"

    it "substitutes an expression for a variable in an expression" $ do
      sub zV y z `shouldBe` y
      sub zV y x `shouldBe` x

    it "ignores expressions that already bind the variable" $ do
      -- such as
      -- (1) lambda abstractions
      let fV = mkVar "f"
      sub fV x (f ~> f) `shouldBe` f ~> f

      -- (2) typecase patterns
      -- tycase x of z:<n> -> z + 1 | y:<t> -> !z(y)
      let tyCase0 = ETyCase x [(Binder z tyInt, EBinOp Add z one), (Binder y tyBool, EUnOp Neg (EApp z y))]
      -- tycase x of z:<n> -> z + 1 | y:<t> -> !(\x.x)(y)
      let tyCase1 = ETyCase x [(Binder z tyInt, EBinOp Add z one), (Binder y tyBool, EUnOp Neg (EApp (f ~> f) y))]
      sub zV (f ~> f) tyCase0 `shouldBe` tyCase1

  describe "rename" $ do
    it "preserves the syntactic identity of variables in a scope" $ do
      -- TODO: write me
      True `shouldBe` False

  describe "renameExprTypes" $ do
    let rename = evalRename' . renameExprTypes

    it "preserves the syntactic identity of type variables in the scope of a lambda binding" $ do
      let tA = mkTv "A"
      let tF = TyFun tA tA
      let lam = (z, tF) +> (z ~> z)

      let (ELam (Binder _ (TyFun t0 t1)) _) = rename lam

      t0 `shouldBe` t1

    it "preserves the syntactic identity of type variables in the scope of a tycase pattern binding" $ do
      let p = ETup [l, r]
      let [tA, tB] = mkTv <$> ["A", "B"]
      let tF = TyFun tA tB
      -- λx . tycase x of (l,r):(<A, B>, <A>) -> l(r) | (l,r):(<A>, <A, B>) -> r(l)
      let tyCase = ETyCase x [(Binder p (TyTup [tF, tA]), l * r), (Binder p (TyTup [tA, tF]), r * l)]

      let (ETyCase _ [(Binder _ (TyTup [tF', tA']), _), (Binder _ (TyTup [tA'', tF'']), _)]) = rename tyCase

      let (TyFun d _) = tF'
      d `shouldBe` tA'

      let (TyFun d _) = tF''
      d `shouldBe` tA''

  describe "alpha equivalence (@=)" $ do
    let (@=) e0 e1 = (e0 C.@= e1) @?= True
    let (!@=) e0 e1 = (e0 C.@= e1) @?= False

    it "recognizes alpha equivalence" $ do
      (x ~> x) @= (y ~> y)
      -- λfλx . f(x) == λfλy . f(y)
      (f ~> (x ~> (f * x))) @= (f ~> (y ~> (f * y)))
      -- λfλx . x(f) == λfλy . y(f)
      (f ~> (x ~> (x * f))) @= (f ~> (y ~> (y * f)))
    it "recognizes syntactic equivalence" $ do
      -- e.g. no renaming is necessary. probably (@=)
      -- should check for syntactic equivalence
      -- before renaming for the sake of economy. tbc
      x @= x
      id @= id
      (x ~> x) @= (x ~> x)
    it "recognizes sets with alpha equivalent members" $ do
      mkS [x ~> x] @= mkS [y ~> y]

    it "recognizes alpha equivalent church trees containing alpha equivalent expression nodes" $ do
      let t0 = mkChurchTree (Node (x ~> x) Leaf Leaf)
      let t1 = mkChurchTree (Node (y ~> y) Leaf Leaf)

      t0 @= t1

    it "doesn't recognize anything else" $ do
      zero !@= one
      -- λfλx . f(x) != λxλf . f(x)
      (f ~> (x ~> (f * x))) !@= (x ~> (f ~> (f * x)))
      -- λfλx . f(x) != λfλx . x(f)
      (f ~> (x ~> (f * x))) !@= (f ~> (x ~> (x * f)))

  describe "desugar" $ do
    it "converts polyadic functions to nested monadic lambdas" $ do
      True `shouldBe` False
    it "converts polyadic applications to nested monadic applications" $ do
      True `shouldBe` False

  describe "reduce" $ do
    let reduce = runReduce :: Expr -> Either ExprEvalError Expr
    let id' y = (x ~> x) * y

    it "reduces equality relations between literals" $ do
      reduce (true === false) `shouldBe` Right false
      reduce (zero === one) `shouldBe` Right false
      reduce (zero === zero) `shouldBe` Right true

    it "reduces equality relations between terms that reduce to literals" $ do
      reduce (id' true === id' false) `shouldBe` Right false

    it "reduces equality relations between nothing else" $ do
      reduce (x === y) `shouldBe` Right (x === y)
      reduce ((x * x) === (x * x)) `shouldBe` Right ((x * x) === (x * x))
      reduce ((x ~> x) === (x ~> x)) `shouldBe` Right ((x ~> x) === (x ~> x))

    it "reduces ternary conditionals over booleans" $ do
      reduce (true ? y > z) `shouldBe` Right y

    it "reduces ternary conditionals over terms that reduce to booleans" $ do
      reduce ((x ~> x) * true ? (y ~> y) > z) `shouldBe` Right (y ~> y)

    it "reduces ternary conditionals over nothing else" $ do
      reduce ((f * x) ? (f * y) > (f * z)) `shouldBe` Right ((f * x) ? (f * y) > (f * z))

    it "reduces arithmetic on terms that reduce to natural numbers" $ do
      reduce (EBinOp Add ((x ~> x) * one) ((x ~> x) * one)) `shouldBe` Right (ELit $ LInt 2)

    it "reduces truth conditional binary operations between set membership" $ do
      let s = mkS [mkI 0, mkI 1]

      reduce ((s * mkI 0) ||| (s * mkI 2)) `shouldBe` Right true
      reduce ((s * mkI 0) &&& (s * mkI 2)) `shouldBe` Right false

      reduce ((s * mkI 0) ||| (s * mkI 1)) `shouldBe` Right true
      reduce ((s * mkI 0) &&& (s * mkI 1)) `shouldBe` Right true

    it "reduces applications of recursive functions" $ do
      --  λn. if n ≤ 1 then 1 else n * fact (n - 1)
      let fact = EFix (mkVar "a") (n ~> EBinOp LTE n (mkI 1) ? mkI 1 > EBinOp Mul n (a * EBinOp Sub n (mkI 1)))

      reduce (fact * mkI 0) `shouldBe` Right (mkI 1)
      reduce (fact * mkI 1) `shouldBe` Right (mkI 1)
      reduce (fact * mkI 2) `shouldBe` Right (mkI 2)
      reduce (fact * mkI 3) `shouldBe` Right (mkI 6)
      reduce (fact * mkI 4) `shouldBe` Right (mkI 24)

    it "reduces set expressions to characteristic functions" $ do
      let s = mkS [mkI 0, mkI 1, mkI 2, mkI 3]

      reduce (s * mkI 0) `shouldBe` Right true
      reduce (s * mkI 1) `shouldBe` Right true
      reduce (s * mkI 2) `shouldBe` Right true
      reduce (s * mkI 3) `shouldBe` Right true

      reduce (s * mkI 4) `shouldBe` Right false

      let s0 = mkS [mkI 0, mkI 1]
      let s1 = mkS [mkI 2, mkI 3]
      let s2 = mkS [mkI 3, mkI 2]
      let s3 = mkS [mkI 3, mkI 2, mkI 4]
      let s = mkS [s0, s1]

      reduce (s * s0) `shouldBe` Right true
      reduce (s * s1) `shouldBe` Right true
      reduce (s * s2) `shouldBe` Right true
      reduce (s * s3) `shouldBe` Right false

    it "reduces case expressions on booleans" $ do
      reduce (ELitCase true [(true, x)]) `shouldBe` Right x
      reduce (ELitCase false [(false, y), (true, x), (false, z)]) `shouldBe` Right y

    it "reduces case expressions on terms that reduce to booleans" $ do
      reduce (ELitCase one [((x ~> x) * one, one), (zero, x), (one, z)]) `shouldBe` Right one

    let id' y = (x ~> x) * y

    it "reduces truth conditional unary operations on booleans" $ do
      reduce (not' true) `shouldBe` Right false
      reduce (not' false) `shouldBe` Right true

    it "reduces truth conditional binary operations on booleans" $ do
      reduce (true &&& false) `shouldBe` Right false
      reduce (true ||| false) `shouldBe` Right true

    it "reduces truth conditional unary operations on terms that reduce to booleans" $ do
      reduce (not' (id' true)) `shouldBe` Right false
      reduce (not' (id' false)) `shouldBe` Right true

    it "reduces truth conditional binary operations on terms that reduce to booleans" $ do
      reduce (id' true &&& id' false) `shouldBe` Right false
      reduce (id' true ||| id' false) `shouldBe` Right true

    it "reduces inequalities between primitives" $ do
      reduce (true !== false) `shouldBe` Right true
      reduce (zero !== one) `shouldBe` Right true

    it "reduces inequalities between terms that reduce to primitives" $ do
      reduce ((true &&& true) !== (false &&& false)) `shouldBe` Right true

    let tup = ETup [id * true, id * false]

    it "reduces the elements of tuples" $ do
      reduce tup `shouldBe` Right (ETup [true, false])

    it "reduces tuple access by index" $ do
      reduce (tup * EIdx 0) `shouldBe` Right true
      reduce (tup * EIdx 1) `shouldBe` Right false

    it "reduces trees to their church encodings" $ do
      let t = ETree (Node f (Node (x ~> x) Leaf Leaf) (Node y Leaf Leaf))

      reduce t `shouldBe` reduce (churchBranch * f * (churchBranch * (x ~> x) * churchLeaf * churchLeaf) * (churchBranch * y * churchLeaf * churchLeaf))

    it "reduces trees in the (terminal) data positions of trees to branches" $ do
      let x = ETree (Node zero (Node zero Leaf Leaf) (Node zero Leaf Leaf))
      let y = ETree (Node one (Node one Leaf Leaf) (Node one Leaf Leaf))
      let t = ETree (Node one (Node x Leaf Leaf) (Node y Leaf Leaf))

      let churchX = churchBranch * zero * (churchBranch * zero * churchLeaf * churchLeaf) * (churchBranch * zero * churchLeaf * churchLeaf)
      let churchY = churchBranch * one * (churchBranch * one * churchLeaf * churchLeaf) * (churchBranch * one * churchLeaf * churchLeaf)
      let churchT = churchBranch * one * churchX * churchY

      reduce t `shouldBe` reduce churchT

    it "reduces typecase expressions with variable bindings" $ do
      let iFn = (+>) (n, tyInt)
      let fnA = iFn (EBinOp Add n one)
      let fnB = iFn (EBinOp Eq n one)

      -- λx . tycase x of z:<n,n> -> z 0 | z:<n,t> -> z 1
      let tyCase = x ~> ETyCase x [(Binder z (TyFun tyInt tyInt), z * zero), (Binder z (TyFun tyInt tyBool), z * one)]

      reduce (tyCase * fnA) `shouldBe` Right one
      reduce (tyCase * fnB) `shouldBe` Right true

      let tyCase = x ~> ETyCase x [(Binder z tyInt, EBinOp Add z one), (Binder z (TyFun tyInt tyInt), z * one)]

      reduce (tyCase * one) `shouldBe` Right (mkI 2)
      reduce (tyCase * fnA) `shouldBe` Right (mkI 2)

    it "reduces typecase expressions with complex bindings" $ do
      let tup0 = ETup [zero, one]
      let tup1 = ETup [zero, one, mkI 2]

      -- λx . tycase x of (y,z):<n + n> -> z + 1 | (x,y,z):<n + n + n> -> z * 1
      let tyCase = x ~> ETyCase x [(Binder (ETup [y, z]) (TyTup [tyInt, tyInt]), EBinOp Add z one), (Binder (ETup [x, y, z]) (TyTup [tyInt, tyInt, tyInt]), EBinOp Mul z one)]

      reduce (tyCase * tup0) `shouldBe` Right (mkI 2)
      reduce (tyCase * tup1) `shouldBe` Right (mkI 2)

    it "reduces generalized quantifiers" $ do
      let par = parse pExpr

      let dom = ESet (Set.fromList (mkI <$> [0, 1, 2, 3, 4, 5]))
      let gq b = q ~> (p ~> b)
      let every r = gq (univ [r] $ (q * x) ==> (p * x))
      let some r = gq (exis [r] $ (q * x) &&& (p * x))
      let (EVar xV) = x

      let (Right lt2) = par "\\x. x <= 2"
      let (Right lt5) = par "\\x. x <= 5"

      reduce (every (xV, dom) * lt2 * lt5) `shouldBe` Right true
      reduce (some (xV, dom) * lt2 * lt5) `shouldBe` Right true

      reduce (every (xV, dom) * lt5 * lt2) `shouldBe` Right false
      reduce (some (xV, dom) * lt5 * lt2) `shouldBe` Right true

  describe "confluence (*=)" $ do
    let (*=) e0 e1 = (e0 E.*= e1) @?= True

    -- λnλfλx . f(n f x)
    let succ = n ~> (f ~> (x ~> (f * (n * f * x))))
    -- λfλx . x
    let zero = f ~> (x ~> x)
    -- λfλx . f x
    let one = f ~> (x ~> (f * x))
    -- λfλx . f(f x)
    let two = f ~> (x ~> (f * (f * x)))
    -- λfλx . f(f(f x))
    let three = f ~> (x ~> (f * (f * (f * x))))
    -- λmλn . m succ n
    let add = m ~> (n ~> (m * succ * n))
    let m + n = add * m * n
    -- λmλnλfλx . m f (n f x)
    let add' = m ~> (n ~> (f ~> (x ~> (m * f * (n * f * x)))))
    let m ++ n = add' * m * n
    -- λmλn . m(add' n)0
    let mul = m ~> (n ~> (m * (add * n) * zero))
    -- λmλn . n(mul m)1
    let exp = m ~> (n ~> (n * (mul * m) * one))
    let m ** n = exp * m * n
    -- λxλy . x
    let true' = x ~> (y ~> x)
    -- λxλy . y
    let false' = x ~> (y ~> y)
    -- λfλxλy . f(x)(y)
    let if' = f ~> (x ~> (y ~> (f * x * y)))
    -- λxλy . if(x)(y)(false)
    let (&&) p q = (x ~> (y ~> (if' * x * y * false'))) * p * q
    -- λxλy . if(x)(true)(y)
    let (||) p q = (x ~> (y ~> (if' * x * true' * y))) * p * q
    -- λxλy . if(x)(true)(false)
    let not' = x ~> (if' * x * false' * true')

    it "equates lambda expressions" $ do
      id * id *= id

    it "equates peano numerals" $ do
      succ * one *= two
      succ * (succ * one) *= three

    it "equates church numerals" $ do
      let m `mul'` n = (mul * m) * n
      let (*) = mul'

      (one + zero) *= one

      (zero + two) *= two
      (two + one) *= three

      (one ++ zero) *= one
      (zero ++ two) *= two
      (two ++ one) *= three

      (zero `mul'` one) *= zero
      (one `mul'` three) *= three
      (two `mul'` three) *= (three + three)
      (two ** one) *= two
      (two ** two) *= (two * two)
      (two ** three) *= (two * two * two)

    it "equates church booleans" $ do
      (true' || false') *= true'
      (false' || true') *= true'
      (true' || true') *= true'
      (false' || false') *= false'

      (true' && false') *= false'
      (false' && true') *= false'
      (false' && false') *= false'
      (true' && true') *= true'

      not' * (true' || false') *= false'
      not' * (false' || true') *= false'
      not' * (true' || true') *= false'
      not' * (false' || false') *= true'

      not' * (true' && false') *= true'
      not' * (false' && true') *= true'
      not' * (false' && false') *= true'
      not' * (true' && true') *= false'

    it "equates folds over trees" $ do
      -- FA composition, l(r)
      -- λxλlλrλz . (l x) (r x)
      let faLR = x ~> (l ~> (r ~> (z ~> ((l * x) * (r * x)))))
      let t = ETree (Node f (Node (x ~> x) Leaf Leaf) (Node y Leaf Leaf))

      (t * churchLeaf * faLR) *= (z ~> ((x ~> x) * y))

      -- FA composition, r(l)
      -- λxλlλrλz . (r x) (l x)
      let faRL = x ~> (l ~> (r ~> (z ~> ((r * x) * (l * x)))))
      let t = ETree (Node f (Node y Leaf Leaf) (Node f (Node y Leaf Leaf) (Node (x ~> (x ~> x)) Leaf Leaf)))

      (t * churchLeaf * faRL) *= (z ~> (((x ~> (x ~> x)) * y) * y))

      -- PA composition, l(r)
      let paLR = faLR
      let t = ETree (Node f (Node (EBind (Binder (mkVar "y") TyNil)) Leaf Leaf) (Node (x ~> x) Leaf Leaf))

      (t * churchLeaf * paLR) *= (z ~> (y ~> (x ~> x)))

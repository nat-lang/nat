module Mean.Evaluation.SurfaceSpec where

import qualified Data.Set as Set
import Debug.Trace (traceM)
import Mean.Evaluation.Surface hiding ((*=), (@=))
import qualified Mean.Evaluation.Surface as E
import Mean.Parser (parse)
import Mean.Syntax.Surface
import Mean.Syntax.Type
import Mean.Unification
import Mean.Var
import Test.HUnit ((@?=))
import Test.Hspec
import Prelude hiding (LTE, id, (&&), (*), (>), (||))

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
  describe "substitution" $ do
    let zV = mkVar "z"

    it "substitutes expressions for variables" $ do
      substitute (mkEnv zV y) z `shouldBe` y

    it "does so discriminately" $ do
      substitute (mkEnv zV y) x `shouldBe` x

    it "avoids variable capture" $ do
      let f1 = EVar $ Var "f0" "f1"

      -- see note in Evaluation.Surface:
      -- (1)
      let fV = mkVar "f"
      substitute (mkEnv fV x) (f ~> f) `shouldBe` f ~> f
      -- (2)
      let nV = mkVar "n"
      substitute (mkEnv nV f) (f ~> (f * n)) `shouldBe` f1 ~> (f1 * f)

  describe "alpha equivalence (@=)" $ do
    let (@=) e0 e1 = (e0 E.@= e1) @?= True
    let (!@=) e0 e1 = (e0 E.@= e1) @?= False

    it "recognizes alpha equivalence" $ do
      (x ~> x) @= (y ~> y)
      -- λfλx . f(x) == λfλy . f(y)
      (f ~> (x ~> (f * x))) @= (f ~> (y ~> (f * y)))
      -- λfλx . x(f) == λfλx . y(f)
      (f ~> (x ~> (x * f))) @= (f ~> (y ~> (y * f)))
    it "recognizes syntactic equivalence" $ do
      -- this is naturally a special case of alpha equivalence,
      -- but probably @= should check for syntactic equivalence
      -- before trying substitutions for the sake of efficiency. tbc
      x @= x
      id @= id
      (x ~> x) @= (x ~> x)
    it "recognizes sets with alpha equivalent members" $ do
      let (Right s0) = eval (mkS [x ~> x])
      let (Right s1) = eval (mkS [y ~> y])

      s0 @= s1

    it "recognizes alpha equivalent church trees containing alpha equivalent expression nodes" $ do
      let (Right t0) = eval (ETree (Node (x ~> x) Leaf Leaf))
      let (Right t1) = eval (ETree (Node (y ~> y) Leaf Leaf))

      t0 @= t1

    it "doesn't recognize anything else" $ do
      x !@= y
      zero !@= one
      -- λfλx . f(x) != λxλf . f(x)
      (f ~> (x ~> (f * x))) !@= (x ~> (f ~> (f * x)))
      -- λfλx . f(x) != λfλx . x(f)
      (f ~> (x ~> (f * x))) !@= (f ~> (x ~> (x * f)))

  describe "eval" $ do
    let id' y = (x ~> x) * y

    it "reduces equality relations between literals" $ do
      eval (true === false) `shouldBe` Right false
      eval (zero === one) `shouldBe` Right false
      eval (zero === zero) `shouldBe` Right true

    it "reduces equality relations between terms that reduce to literals" $ do
      eval (id' true === id' false) `shouldBe` Right false

    it "reduces equality relations between nothing else" $ do
      eval (x === y) `shouldBe` Right (x === y)
      eval ((x * x) === (x * x)) `shouldBe` Right ((x * x) === (x * x))
      eval ((x ~> x) === (x ~> x)) `shouldBe` Right ((x ~> x) === (x ~> x))

    it "reduces ternary conditionals on booleans" $ do
      eval (true ? y > z) `shouldBe` Right y

    it "reduces ternary conditionals on terms that reduce to booleans" $ do
      eval ((x ~> x) * true ? (y ~> y) > z) `shouldBe` Right (y ~> y)

    it "reduces ternary conditionals over nothing else" $ do
      eval ((f * x) ? (f * y) > (f * z)) `shouldBe` Right ((f * x) ? (f * y) > (f * z))

    it "reduces arithmetic on terms that reduce to natural numbers" $ do
      eval (EBinOp Add ((x ~> x) * one) ((x ~> x) * one)) `shouldBe` Right (ELit $ LInt 2)

    it "reduces truth conditional binary operations between set membership" $ do
      let s = mkS [mkI 0, mkI 1]

      eval ((s * mkI 0) || (s * mkI 2)) `shouldBe` Right true
      eval ((s * mkI 0) && (s * mkI 2)) `shouldBe` Right false

      eval ((s * mkI 0) || (s * mkI 1)) `shouldBe` Right true
      eval ((s * mkI 0) && (s * mkI 1)) `shouldBe` Right true

    it "reduces applications of recursive functions" $ do
      --  λn. if n ≤ 1 then 1 else n * fact (n - 1)
      let fact = EFix (mkVar "a") (n ~> EBinOp LTE n (mkI 1) ? mkI 1 > EBinOp Mul n (a * EBinOp Sub n (mkI 1)))

      eval (fact * mkI 0) `shouldBe` Right (mkI 1)
      eval (fact * mkI 1) `shouldBe` Right (mkI 1)
      eval (fact * mkI 2) `shouldBe` Right (mkI 2)
      eval (fact * mkI 3) `shouldBe` Right (mkI 6)
      eval (fact * mkI 4) `shouldBe` Right (mkI 24)

    it "reduces set expressions to characteristic functions" $ do
      let s = mkS [mkI 0, mkI 1, mkI 2, mkI 3]

      eval (s * mkI 0) `shouldBe` Right true
      eval (s * mkI 1) `shouldBe` Right true
      eval (s * mkI 2) `shouldBe` Right true
      eval (s * mkI 3) `shouldBe` Right true

      eval (s * mkI 4) `shouldBe` Right false

      let s0 = mkS [mkI 0, mkI 1]
      let s1 = mkS [mkI 2, mkI 3]
      let s2 = mkS [mkI 3, mkI 2]
      let s3 = mkS [mkI 3, mkI 2, mkI 4]
      let s = mkS [s0, s1]

      eval (s * s0) `shouldBe` Right true
      eval (s * s1) `shouldBe` Right true
      eval (s * s2) `shouldBe` Right true
      eval (s * s3) `shouldBe` Right false

    it "reduces trees to their church encodings" $ do
      let t = ETree (Node f (Node (x ~> x) Leaf Leaf) (Node y Leaf Leaf))

      eval t `shouldBe` eval (churchNode * f * (churchNode * (x ~> x) * churchLeaf * churchLeaf) * (churchNode * y * churchLeaf * churchLeaf))

    it "reduces case expressions on booleans" $ do
      eval (ELitCase true [(true, x)]) `shouldBe` Right x
      eval (ELitCase false [(false, y), (true, x), (false, z)]) `shouldBe` Right y

    it "reduces case expressions on terms that reduce to booleans" $ do
      eval (ELitCase one [((x ~> x) * one, one), (zero, x), (one, z)]) `shouldBe` Right one

    let id' y = (x ~> x) * y

    it "reduces truth conditional unary operations on booleans" $ do
      eval (not' true) `shouldBe` Right false
      eval (not' false) `shouldBe` Right true

    it "reduces truth conditional binary operations on booleans" $ do
      eval (true && false) `shouldBe` Right false
      eval (true || false) `shouldBe` Right true

    it "reduces truth conditional unary operations on terms that reduce to booleans" $ do
      eval (not' (id' true)) `shouldBe` Right false
      eval (not' (id' false)) `shouldBe` Right true

    it "reduces truth conditional binary operations on terms that reduce to booleans" $ do
      eval (id' true && id' false) `shouldBe` Right false
      eval (id' true || id' false) `shouldBe` Right true

    it "reduces inequalities between primitives" $ do
      eval (true !== false) `shouldBe` Right true
      eval (x !== y) `shouldBe` Right true
      eval (zero !== one) `shouldBe` Right true

    it "reduces inequalities between relations" $ do
      eval ((true && true) !== (false && false)) `shouldBe` Right true

    it "reduces type case expressions" $ do
      let iFn = (+>) (n, tyInt)
      let fnA = iFn (EBinOp Add n one)
      let fnB = iFn (EBinOp Eq n one)

      let tyCase = x ~> ETyCase x [(Binder z (TyFun tyInt tyInt), z * zero), (Binder z (TyFun tyInt tyBool), z * one)]

      eval (tyCase * fnA) `shouldBe` Right one
      eval (tyCase * fnB) `shouldBe` Right true

      let tyCase = x ~> ETyCase x [(Binder z tyInt, EBinOp Add z one), (Binder z (TyFun tyInt tyInt), z * one)]

      eval (tyCase * one) `shouldBe` Right (mkI 2)
      eval (tyCase * fnA) `shouldBe` Right (mkI 2)

  -- let tyCase = x ~> ETyCase x [(Binder (ETup [y, z]) (TyTup [tyInt, tyInt]), EBinOp Add z one), (Binder (ETup [x, y, z]) (TyTup [tyInt, tyInt, tyInt]), z * one)]
  -- let tup0 = ETup [zero, one]
  -- let tup1 = ETup [one, zero, mkI 2]

  -- eval (tyCase * tup0) `shouldBe` Right (mkI 2)
  -- eval (tyCase * tup1) `shouldBe` Right (mkI 2)

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

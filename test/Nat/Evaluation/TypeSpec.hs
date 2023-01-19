{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Nat.Evaluation.TypeSpec where

import Data.List (foldl')
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace (traceM)
import Nat.Context
import Nat.Evaluation.Surface
import Nat.Evaluation.Type
import Nat.Inference
import Nat.Inference (Inferrable (runSignifyIn))
import Nat.Parser
import Nat.Syntax.Surface hiding (fromList)
import Nat.Syntax.Type
import Nat.Unification
import Test.Hspec
import Prelude hiding ((*), (>))

mkTypeEnv :: [(Var, Type)] -> TypeEnv
mkTypeEnv = foldl' extend mempty

fn x ty = ELam (Binder (mkVar x) ty)

tyA = mkTv "A"

tyB = mkTv "B"

[x, y, z, w] = mkEVar <$> ["x", "y", "z", "w"]

(EVar xV) = x

(EVar yV) = y

(EVar zV) = z

(EVar wV) = w

true = ELit $ LBool True

false = ELit $ LBool False

zero = ELit $ LInt 0

one = ELit $ LInt 1

spec :: Spec
spec = do
  describe "infer" $ do
    it "infers the type of literal integers" $ do
      infer (ELit $ LInt 0) `shouldBe` Right tyInt
    it "infers the type of literal booleans" $ do
      infer (ELit $ LBool True) `shouldBe` Right tyBool

    it "infers the types of functions" $ do
      let inferFnTy ty = infer (fn "x" ty x) `shouldBe` Right (TyFun ty ty)

      inferFnTy tyBool
      inferFnTy tyInt

    it "infers the types of function applications" $ do
      let inferAppTy ty = inferIn (mkTypeEnv [(yV, ty')]) (fn "x" ty x * y) `shouldBe` Right ty' where ty' = ty

      inferAppTy tyBool
      inferAppTy tyInt

    it "infers the type of equalities" $ do
      infer (one === one) `shouldBe` Right tyBool
      infer (one === zero) `shouldBe` Right tyBool
      infer (true === true) `shouldBe` Right tyBool
      infer (true === false) `shouldBe` Right tyBool

    it "enforces constraints on type variables" $ do
      let env = mkTypeEnv [(yV, tyA)]
      let (Right ty) = inferIn env (fn "x" tyInt x * y)

      ty `shouldBe` tyInt

    it "enforces constraints on equalities" $ do
      let env = mkTypeEnv [(xV, tyA), (yV, tyB)]
      let (Right sub) = runSignifyIn (x === y) env

      inEnv sub tyA `shouldBe` inEnv sub tyB

    it "enforces constraints on ternary conditionals" $ do
      let tyC = mkTv "C"

      let env = mkTypeEnv [(xV, tyA), (yV, tyB), (zV, tyC)]
      let (Right (_, sub, ty)) = constraintsIn (x ? y > z) env

      inEnv sub tyA `shouldBe` tyBool
      inEnv sub tyB `shouldBe` ty
      inEnv sub tyC `shouldBe` ty

    it "types recursive functions" $ do
      let iE = ELit . LInt
      let n = mkEVar "n"
      let f = mkEVar "f"
      -- y = λf. (λx . f (x x)) (λx . f (x x))
      -- y * (λfλn. if n ≤ 1 then 1 else n * f (n - 1))
      let fact = EFix (mkVar "f") (n ~> ECond (EBinOp LTE n (iE 1)) (iE 1) (EBinOp Mul n (EApp f (EBinOp Sub n (iE 1)))))
      infer fact `shouldBe` Right (TyFun tyInt tyInt)

    it "types abstractions over trees" $ do
      let (Right t) = parse pExpr "\\a. [a [1] [0]]"

      infer t `shouldBe` Right (mkTv "a")

    it "types folds over trees" $ do
      let (Right t) = parse pExpr "[0 [1] [0]]"
      let (Right f) = parse pExpr "\\x:<X>.\\l:<L>.\\r:<R>. x + l + r"

      let ty = infer (t * zero * f)

      ty `shouldBe` Right tyInt

    it "infers the domain of the branches of a church tree with polymorphic nodes to be a union type" $ do
      let (Right t) = parse pExpr "[(\\x.x) [True] [0]]"
      case infer t :: Either (InferenceError Type Expr) Type of
        Left e -> traceM (show e)
        Right t' -> do
          let (TyFun _ (TyFun (TyFun ty _) _)) = t'
          let isTyUnion = \case TyUnion {} -> True; _ -> False
          let tA = mkTv "A"

          isTyUnion ty `shouldBe` True
          ty <=> TyFun tA tA `shouldBe` True
          ty <=> tyInt `shouldBe` True
          ty <=> tyBool `shouldBe` True

    it "types parametric polymorphic functions" $ do
      let polyId = (y ~> ((y * true) ? (y * zero) > (y * one))) * (x ~> x)

      infer polyId `shouldBe` Right tyInt

  describe "the typecase expression" $ do
    -- these tests can be less circuitous when we have type
    -- annotations for terms other than binders
    let env = Map.fromList [(xV, tyInt)]

    it "has the type of its matched case body" $ do
      let cases = [(Binder z tyInt, z === zero), (Binder z tyBool, z ? one > zero)]

      inferIn env (ETyCase x cases) `shouldBe` Right tyBool

    it "propagates the constraints of its case bodies" $ do
      let tyCase = ETyCase x [(Binder z tyInt, EBinOp Add z y), (Binder z tyBool, EBinOp Eq z w ? one > zero)]

      let tB = mkTv "B"
      let tC = mkTv "C"
      let env' = Map.union env $ Map.fromList [(yV, tB), (wV, tC)]
      let (Right (_, sub, _)) = constraintsIn tyCase env'

      inEnv sub tB `shouldBe` tyInt
      inEnv sub tC `shouldBe` tyBool

    let cases = [(Binder z tyInt, z === zero), (Binder z tyBool, z)]

    it "propagates the constraints of its argument" $ do
      let tA = mkTv "A"
      let env' = extend env (yV, tA)
      let (Right (cs, sub, t)) = constraintsIn (ETyCase (EBinOp Add x y) cases) env'

      inEnv sub tA `shouldBe` tyInt

    it "constrains its argument to be the union of the pattern types of its cases" $ do
      let unionTy = mkTyUnion [tyInt, tyBool]
      let (Right (TyFun argTy _)) = infer (x ~> ETyCase x cases)

      argTy `shouldBe` unionTy

    it "types tuple patterns" $ do
      let [l@(EVar lV), r@(EVar rV)] = mkEVar <$> ["l", "r"]
      let (Right tyCase) = parse pExpr "tycase (l, r) of (l',r'):(<X,Y>, <X>) -> l'(r') | (l',r'):(<X>, <X,Y>) -> r'(l') | _ -> (l,r)"

      let env = Map.fromList [(lV, TyFun tyInt tyBool), (rV, tyInt)]
      inferIn env tyCase `shouldBe` Right tyBool

      let env = Map.fromList [(rV, TyFun tyInt tyBool), (lV, tyInt)]
      inferIn env tyCase `shouldBe` Right tyBool

      let env = Map.fromList [(rV, tyInt), (lV, tyInt)]
      inferIn env tyCase `shouldBe` Right (TyTup [tyInt, tyInt])

  describe "the union type" $ do
    it "is unified with other types by membership" $ do
      let [tA, tB, tC] = TyCon . mkVar <$> ["A", "B", "C"]
      let unionTy = mkTyUnion [tA, tB]

      unionTy <=> tA `shouldBe` True
      unionTy <=> tB `shouldBe` True
      unionTy <=> tC `shouldBe` False

    it "is inferrable" $ do
      let (Right expr) = parse pExpr "\\f. (f 1) && (f True)"

      infer expr `shouldBe` Right (TyFun (TyFun (mkTyUnion [tyInt, tyBool]) tyBool) tyBool)
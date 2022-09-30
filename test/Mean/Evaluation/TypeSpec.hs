{-# LANGUAGE OverloadedStrings #-}

module Mean.Evaluation.TypeSpec where

import Data.List (foldl')
import qualified Data.Set as Set
import Debug.Trace (traceM)
import Mean.Evaluation.Surface hiding (substitute)
import Mean.Evaluation.Type hiding (mkTyEnv)
import Mean.Syntax.Surface hiding (fromList)
import Mean.Syntax.Type
import Mean.Unification
import Mean.Var
import Test.Hspec
import Prelude hiding ((*), (>))

mkTyEnv :: [(Var, TyScheme)] -> TyEnv
mkTyEnv = foldl' extend mempty

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
  describe "constraintsOnExpr" $ do
    it "enforces constraints on type variables" $ do
      let env = mkTyEnv [(yV, mkUnqScheme tyA)]
      let (Right (_, sub, ty, _)) = constraintsOnExpr' env (fn "x" tyInt x * y)

      substitute sub ty `shouldBe` tyInt

  describe "infer" $ do
    it "infers the type of literal integers" $ do
      infer (ELit $ LInt 0) `shouldBe` Right (mkUnqScheme tyInt)
    it "infers the type of literal booleans" $ do
      infer (ELit $ LBool True) `shouldBe` Right (mkUnqScheme tyBool)

    it "infers the types of functions" $ do
      let inferFnTy ty = infer (fn "x" ty x) `shouldBe` Right (mkUnqScheme (TyFun ty ty))

      inferFnTy tyBool
      inferFnTy tyInt

    it "infers the types of function applications" $ do
      let inferAppTy ty = infer' (mkTyEnv [(yV, ty')]) ((fn "x" ty x) * y) `shouldBe` Right ty' where ty' = mkUnqScheme ty

      inferAppTy tyBool
      inferAppTy tyInt

    it "infers the type of equalities" $ do
      let tb = mkUnqScheme tyBool
      infer (one === one) `shouldBe` Right tb
      infer (one === zero) `shouldBe` Right tb
      infer (true === true) `shouldBe` Right tb
      infer (true === false) `shouldBe` Right tb

    it "enforces constraints on equalities" $ do
      let env = mkTyEnv [(xV, mkUnqScheme tyA), (yV, mkUnqScheme tyB)]
      let (Right (_, sub, _, _)) = constraintsOnExpr' env (x === y)

      substitute sub tyA `shouldBe` substitute sub tyB

    it "enforces constraints on ternary conditionals" $ do
      let tyC = mkTv "C"

      let env = mkTyEnv [(xV, mkUnqScheme tyA), (yV, mkUnqScheme tyB), (zV, mkUnqScheme tyC)]
      let (Right (_, sub, ty, _)) = constraintsOnExpr' env (x ? y > z)

      substitute sub tyA `shouldBe` tyBool
      substitute sub tyB `shouldBe` ty
      substitute sub tyC `shouldBe` ty

    it "types recursive functions" $ do
      let iE = ELit . LInt
      let n = mkEVar "n"
      let a = mkEVar "a"
      --  λn. if n ≤ 1 then 1 else n * fact (n - 1)
      let fact = EFix (mkVar "a") (n ~> EBinOp LTE n (iE 1) ? iE 1 > EBinOp Mul n (a * EBinOp Sub n (iE 1)))
      let (Right fact') = eval fact

      infer (fact' * one) `shouldBe` Right (mkUnqScheme tyInt)

  describe "the typecase expression" $ do
    -- these tests can be less circuitous when we have type
    -- annotations for terms other than binders
    let tA = mkTv "A"
    let env = fromList [(xV, mkUnqScheme tA)]

    let incompatibleCases = [(Binder z tyInt, z === zero), (Binder z tyBool, z ? one > zero)]
    let compatibleCases = [(Binder z tyInt, z === zero), (Binder z tyBool, z)]

    it "constrains case bodies to have the same type" $ do
      infer' env (ETyCase x compatibleCases) `shouldBe` Right (mkUnqScheme tyBool)
      infer' env (ETyCase x incompatibleCases) `shouldBe` Left (TyUnificationError $ NotUnifiable tyBool tyInt)

    it "propagates the constraints of its case bodies" $ do
      let tB = mkTv "B"
      let tC = mkTv "C"
      let env' = merge env $ fromList [(yV, mkUnqScheme tB), (wV, mkUnqScheme tC)]
      let tyCase = ETyCase x [(Binder z tyInt, EBinOp Add z y), (Binder z tyBool, EBinOp Eq z w ? one > zero)]
      let (Right (_, sub, _, _)) = constraintsOnExpr' env' tyCase

      substitute sub tB `shouldBe` tyInt
      substitute sub tC `shouldBe` tyBool

    it "propagates the constraints of its argument" $ do
      let env' = extend env (yV, mkUnqScheme tyInt)
      let (Right (cs, sub, t, ts)) = constraintsOnExpr' env' (ETyCase (EBinOp Add x y) compatibleCases)

      substitute sub tA `shouldBe` tyInt

    it "constrains its argument to be the union of the type patterns of its cases" $ do
      let unionTy = mkTyUnion [tyInt, tyBool]
      infer (x ~> ETyCase x compatibleCases) `shouldBe` Right (mkUnqScheme $ TyFun unionTy tyBool)

  describe "the union type" $ do
    it "is unified with other types existentially" $ do
      let [tA, tB, tC] = TyCon <$> ["A", "B", "C"]
      let unionTy = mkTyUnion [tA, tB]

      unionTy <=> tA `shouldBe` True
      unionTy <=> tB `shouldBe` True
      unionTy <=> tC `shouldBe` False

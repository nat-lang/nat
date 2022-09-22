{-# LANGUAGE OverloadedStrings #-}

module Mean.Evaluation.TypeSpec where

import Data.List (foldl')
import qualified Data.Set as Set
import Debug.Trace (traceM)
import Mean.Evaluation.Surface hiding (substitute)
import Mean.Evaluation.Type hiding (Substitution (..))
import Mean.Syntax.Surface
import Mean.Syntax.Type
import Test.Hspec
import Prelude hiding ((*), (>))

mkEnv :: [(Name, TyScheme)] -> TyEnv
mkEnv = foldl' extend mempty

fn x ty y = ELam (Binder (mkVar x) ty) y

tyA = mkTv "A"

tyB = mkTv "B"

[x, y, z, w] = mkEVar <$> ["x", "y", "z", "w"]

(EVar (Var _ xPri)) = x

(EVar (Var _ yPri)) = y

true = ELit $ LBool True

false = ELit $ LBool False

zero = ELit $ LInt 0

one = ELit $ LInt 1

spec :: Spec
spec = do
  describe "constraintsOnExpr" $ do
    it "enforces constraints on type variables" $ do
      let env = mkEnv [(yPri, mkUnqScheme tyA)]
      let (Right (_, sub, ty, _)) = constraintsOnExpr' env (fn "x" tyInt x * y)

      substitute sub ty `shouldBe` tyInt

  describe "infer" $ do
    it "types literal integers" $ do
      infer (ELit $ LInt 0) `shouldBe` Right (mkUnqScheme tyInt)
    it "types literal booleans" $ do
      infer (ELit $ LBool True) `shouldBe` Right (mkUnqScheme tyBool)

    it "infers the types of functions" $ do
      let inferFnTy ty = infer (fn "x" ty x) `shouldBe` Right (mkUnqScheme (TyFun ty ty))

      inferFnTy tyBool
      inferFnTy tyInt

    it "infers the types of function applications" $ do
      let inferAppTy ty = infer' (mkEnv [(yPri, ty')]) ((fn "x" ty x) * y) `shouldBe` Right ty' where ty' = mkUnqScheme ty

      inferAppTy tyBool
      inferAppTy tyInt

    it "types equalities" $ do
      let tb = mkUnqScheme tyBool
      infer (one === one) `shouldBe` Right tb
      infer (one === zero) `shouldBe` Right tb
      infer (true === true) `shouldBe` Right tb
      infer (true === false) `shouldBe` Right tb

    it "enforces constraints on equalities" $ do
      let env = mkEnv [(xPri, mkUnqScheme tyA), (yPri, mkUnqScheme tyB)]
      let (Right (_, sub, _, _)) = constraintsOnExpr' env (x === y)

      substitute sub tyA `shouldBe` substitute sub tyB

    it "enforces constraints on ternary conditionals" $ do
      let (EVar (Var _ zPri)) = mkEVar "z"
      let tyC = mkTv "C"

      let env = mkEnv [(xPri, mkUnqScheme tyA), (yPri, mkUnqScheme tyB), (zPri, mkUnqScheme tyC)]
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
    let (EVar zV) = z
    let env = fromList [(xPri, mkUnqScheme tA)]

    let incompatibleCases = [(Binder zV tyInt, z === zero), (Binder zV tyBool, z ? one > zero)]
    let compatibleCases = [(Binder zV tyInt, z === zero), (Binder zV tyBool, z)]

    it "constrains case bodies to have the same type" $ do
      infer' env (ETyCase x compatibleCases) `shouldBe` Right (mkUnqScheme tyBool)
      infer' env (ETyCase x incompatibleCases) `shouldBe` Left (NotUnifiable tyBool tyInt)

    it "propagates the constraints of its case bodies" $ do
      let (EVar (Var _ wPri)) = w
      let tB = mkTv "B"
      let tC = mkTv "C"
      let env' = merge env $ fromList [(yPri, mkUnqScheme tB), (wPri, mkUnqScheme tC)]
      let tyCase = ETyCase x [(Binder zV tyInt, EBinOp Add z y), (Binder zV tyBool, EBinOp Eq z w ? one > zero)]
      let (Right (_, sub, _, _)) = constraintsOnExpr' env' tyCase

      substitute sub tB `shouldBe` tyInt
      substitute sub tC `shouldBe` tyBool

    it "propagates the constraints of its argument" $ do
      let env' = extend env (yPri, mkUnqScheme tyInt)
      let (Right (_, sub, _, _)) = constraintsOnExpr' env' (ETyCase (EBinOp Add x y) compatibleCases)

      substitute sub tA `shouldBe` tyInt

    it "constrains its argument to be the union of the type patterns of its cases" $ do
      let unionTy = mkTyUnion [tyInt, tyBool]
      infer (x ~> ETyCase x compatibleCases) `shouldBe` Right (mkUnqScheme $ TyFun unionTy tyBool)

  describe "the union type" $ do
    it "is unified with other types existentially" $ do
      True `shouldBe` False

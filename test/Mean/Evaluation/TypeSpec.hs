{-# LANGUAGE OverloadedStrings #-}

module Mean.Evaluation.TypeSpec where

import Data.List (foldl')
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace (traceM)
import Mean.Evaluation.Surface hiding (substitute)
import Mean.Evaluation.Type
import Mean.Inference
import Mean.Parser
import Mean.Syntax.Surface hiding (fromList)
import Mean.Syntax.Type
import Mean.Unification
import Mean.Unification (Substitutable (substitute))
import Mean.Var
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
  describe "constraintsOn" $ do
    it "enforces constraints on type variables" $ do
      let env = mkTypeEnv [(yV, tyA)]
      let (Right (_, sub, ty)) = constraintsIn env (fn "x" tyInt x * y)

      substitute sub ty `shouldBe` tyInt

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
      let inferAppTy ty = inferIn (mkTypeEnv [(yV, ty')]) ((fn "x" ty x) * y) `shouldBe` Right ty' where ty' = ty

      inferAppTy tyBool
      inferAppTy tyInt

    it "infers the type of equalities" $ do
      let tb = tyBool
      infer (one === one) `shouldBe` Right tb
      infer (one === zero) `shouldBe` Right tb
      infer (true === true) `shouldBe` Right tb
      infer (true === false) `shouldBe` Right tb

    it "enforces constraints on equalities" $ do
      let env = mkTypeEnv [(xV, tyA), (yV, tyB)]
      let (Right (_, sub, _)) = constraintsIn env (x === y)

      substitute sub tyA `shouldBe` substitute sub tyB

    it "enforces constraints on ternary conditionals" $ do
      let tyC = mkTv "C"

      let env = mkTypeEnv [(xV, tyA), (yV, tyB), (zV, tyC)]
      let (Right (_, sub, ty)) = constraintsIn env (x ? y > z)

      substitute sub tyA `shouldBe` tyBool
      substitute sub tyB `shouldBe` ty
      substitute sub tyC `shouldBe` ty

    it "types recursive functions" $ do
      let iE = ELit . LInt
      let n = mkEVar "n"
      let f = mkEVar "f"
      -- y = λf. (λx . f (x x)) (λx . f (x x))
      -- y * (λfλn. if n ≤ 1 then 1 else n * f (n - 1))
      let fact = EFix (mkVar "f") (n ~> EBinOp LTE n (iE 1) ? iE 1 > EBinOp Mul n (f * EBinOp Sub n (iE 1)))

      let (Right (cs, env, t)) = constraints (fact * one)
      traceM ("\nconstraints: " ++ show cs)
      traceM ("\nenv: " ++ show env)
      t `shouldBe` tyInt

    it "types the branches of trees with polymorphic nodes as unions" $ do
      let (Right t) = parse pExpr "[(\\x.x) [True] [0]]"
      let (Right (TyFun _ (TyFun ty _))) = infer t
      let tA = mkTv "A"
      ty `shouldBe` TyUnion (Set.fromList [tyBool, TyFun tA tA, tyInt])

    it "types parametric polymorphic functions" $ do
      let polyId = (y ~> ((y * true) ? (y * zero) > (y * one))) * (x ~> x)

      infer polyId `shouldBe` Right tyInt

  describe "the typecase expression" $ do
    -- these tests can be less circuitous when we have type
    -- annotations for terms other than binders
    let tA = mkTv "A"
    let env = Map.fromList [(xV, tyInt)]

    it "has the type of its matched case body" $ do
      let cases = [(Binder z tyInt, z === zero), (Binder z tyBool, z ? one > zero)]

      inferIn env (ETyCase x cases) `shouldBe` Right tyBool

    it "propagates the constraints of its matched case body" $ do
      let tyCase = ETyCase x [(Binder z tyInt, EBinOp Add z y), (Binder z tyBool, EBinOp Eq z w ? one > zero)]

      let tB = mkTv "B"
      let env' = Map.union env $ Map.singleton yV tB
      let (Right (_, sub, _)) = constraintsIn env' tyCase
      substitute sub tB `shouldBe` tyInt

      let tC = mkTv "C"
      let env' = Map.fromList [(xV, tyBool), (wV, tC)]
      let (Right (_, sub, _)) = constraintsIn env' tyCase
      substitute sub tC `shouldBe` tyBool

    let cases = [(Binder z tyInt, z === zero), (Binder z tyBool, z)]

    it "propagates the constraints of its argument" $ do
      let env' = extend env (yV, tyInt)
      let (Right (_, sub, _)) = constraintsIn env' (ETyCase (EBinOp Add x y) cases)

      substitute sub tA `shouldBe` tyInt

    it "constrains its argument to be the union of the pattern types of its cases" $ do
      let unionTy = mkTyUnion [tyInt, tyBool]
      let (Right (TyFun argTy _)) = infer (x ~> ETyCase x cases)

      argTy `shouldBe` unionTy

    it "types tuple patterns" $ do
      let [l@(EVar lV), r@(EVar rV)] = mkEVar <$> ["l", "r"]
      let (Right tyCase) = parse pExpr "tycase (l, r) of (l',r'):(<A,B>, <A>) -> l'(r') | (l',r'):(<A>, <A,B>) -> r'(l') | _ -> (l,r)"

      let env = Map.fromList [(lV, TyFun tyInt tyBool), (rV, tyInt)]
      inferIn env tyCase `shouldBe` Right tyBool

      let env = Map.fromList [(rV, TyFun tyInt tyBool), (lV, tyInt)]
      inferIn env tyCase `shouldBe` Right tyBool

      let env = Map.fromList [(rV, tyInt), (lV, tyInt)]
      inferIn env tyCase `shouldBe` Right (TyTup [tyInt, tyInt])

  describe "the union type" $ do
    it "is unified with other types existentially" $ do
      let [tA, tB, tC] = TyCon <$> ["A", "B", "C"]
      let unionTy = mkTyUnion [tA, tB]

      unionTy <=> tA `shouldBe` True
      unionTy <=> tB `shouldBe` True
      unionTy <=> tC `shouldBe` False

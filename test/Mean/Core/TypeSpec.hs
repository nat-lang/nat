{-# LANGUAGE OverloadedStrings #-}

module Mean.Core.TypeSpec where

import Debug.Trace (traceM)
import Mean.Core
import Mean.Type
import Test.Hspec
import Prelude hiding ((>))

spec :: Spec
spec = do
  let inf = inferExpr empty
  let fn x ty y = mkCLam (Binder (mkVar x) ty) y
  let (CVar (Var _ xPri)) = mkCVar "x"
  let (CVar (Var _ yPri)) = mkCVar "y"
  let tyA = mkTv "A"
  let tyB = mkTv "B"

  describe "constraintsOnExpr" $ do
    it "enforces constraints on type variables" $ do
      let env = mkEnv [(yPri, mkUnqScheme tyA)]
      let (Right (_, sub, ty, _)) = constraintsOnExpr env (mkCApp (fn "x" tyInt x) y)

      apply sub ty `shouldBe` tyInt

  describe "inferExpr" $ do
    it "types literal integers" $ do
      inf (CLit $ LInt 0) `shouldBe` Right (mkUnqScheme tyInt)
    it "types literal booleans" $ do
      inf (CLit $ LBool True) `shouldBe` Right (mkUnqScheme tyBool)

    it "infers the types of functions" $ do
      let inferFnTy ty = inf (fn "x" ty x) `shouldBe` Right (mkUnqScheme (TyFun ty ty))

      inferFnTy tyBool
      inferFnTy tyInt

    it "infers the types of function applications" $ do
      let inferAppTy ty = inferExpr (mkEnv [(yPri, ty')]) (mkCApp (fn "x" ty x) y) `shouldBe` Right ty' where ty' = mkUnqScheme ty

      inferAppTy tyBool
      inferAppTy tyInt
    
    it "types equalities" $ do
      let tb = mkUnqScheme tyBool
      inf (one === one) `shouldBe` Right tb
      inf (one === zero) `shouldBe` Right tb
      inf (true === true) `shouldBe` Right tb
      inf (true === false) `shouldBe` Right tb

    it "enforces constraints on equalities" $ do
      let env = mkEnv [(xPri, mkUnqScheme tyA), (yPri, mkUnqScheme tyB)]
      let (Right (_, sub, _, _)) = constraintsOnExpr env (x === y)

      apply sub tyA `shouldBe` apply sub tyB
    
    it "enforces constraints on ternary conditionals" $ do
      let (CVar (Var _ zPri)) = mkCVar "z"
      let tyC = mkTv "C"

      let env = mkEnv [(xPri, mkUnqScheme tyA), (yPri, mkUnqScheme tyB), (zPri, mkUnqScheme tyC)]
      let (Right (_, sub, ty, _)) = constraintsOnExpr env (x ? y > z)

      apply sub tyA `shouldBe` tyBool
      apply sub tyB `shouldBe` ty
      apply sub tyC `shouldBe` ty


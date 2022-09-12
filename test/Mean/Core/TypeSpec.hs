{-# LANGUAGE OverloadedStrings #-}

module Mean.Core.TypeSpec where

import Mean.Core
import Mean.Type
import Test.Hspec

spec :: Spec
spec = do
  let inEnv tyAss = extend empty tyAss
  let fn x ty y = mkCLam (Binder (mkVar x) ty) y
  let x = mkCVar "x"
  let y@(CVar (Var _ yPri)) = mkCVar "y"
  let tyA = mkTv "A"

  describe "constraintsOnExpr" $ do
    it "enforces constraints on type variables" $ do
      let env = inEnv (yPri, mkUnqScheme tyA)
      let (Right (_, sub, ty, _)) = constraintsOnExpr env (mkCApp (fn "x" tyInt x) y)

      apply sub ty `shouldBe` tyInt

  describe "inferExpr" $ do
    it "types literal integers" $ do
      inferExpr empty (CLit $ LInt 0) `shouldBe` Right (mkUnqScheme tyInt)
    it "types literal booleans" $ do
      inferExpr empty (CLit $ LBool True) `shouldBe` Right (mkUnqScheme tyBool)

    it "infers the types of functions" $ do
      let inferFnTy ty = inferExpr empty (fn "x" ty x) `shouldBe` Right (mkUnqScheme (TyFun ty ty))

      inferFnTy tyBool
      inferFnTy tyInt

    it "infers the types of function applications" $ do
      let inferAppTy ty = inferExpr (inEnv (yPri, ty')) (mkCApp (fn "x" ty x) y) `shouldBe` Right ty' where ty' = mkUnqScheme ty

      inferAppTy tyBool
      inferAppTy tyInt

{-# LANGUAGE OverloadedStrings #-}

module Mean.TypeSpec where

import Mean.Parser
import Mean.Syntax
import Mean.Type
import Mean.TypeEnv
import Mean.Viz
import Test.Hspec

spec :: Spec
spec = do
  let inEnv tyAss = extend empty tyAss
  let fn x ty y = Lam (Binder (mkVar x) ty) y
  let x = mkEVar "x"
  let y@(EVar (Var _ yPri)) = mkEVar "y"
  let tyA = mkTv "A"

  describe "constraintsOnExpr" $ do
    it "enforces constraints on type variables" $ do
      let env = inEnv (yPri, mkUnqScheme tyA)
      let (Right (_, sub, ty, _)) = constraintsOnExpr env (App (fn "x" tyInt x) y)

      apply sub ty `shouldBe` tyInt

  describe "inferExpr" $ do
    it "types literal integers" $ do
      inferExpr empty (ELit $ LInt 0) `shouldBe` Right (mkUnqScheme tyInt)
    it "types literal booleans" $ do
      inferExpr empty (ELit $ LBool True) `shouldBe` Right (mkUnqScheme tyBool)

    it "infers the types of functions" $ do
      let inferFnTy ty = inferExpr empty (fn "x" ty x) `shouldBe` Right (mkUnqScheme (TyFun ty ty))

      inferFnTy tyBool
      inferFnTy tyInt

    it "infers the types of function applications" $ do
      let inferAppTy ty = inferExpr (inEnv (yPri, ty')) (App (fn "x" ty x) y) `shouldBe` Right ty' where ty' = mkUnqScheme ty

      inferAppTy tyBool
      inferAppTy tyInt

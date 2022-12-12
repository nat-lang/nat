{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Nat.Syntax.ModuleSpec where

import qualified Data.Set as Set
import Nat.Context
import Nat.Parser (parse)
import Nat.Syntax.Module
import Nat.Syntax.Surface
import Nat.Syntax.Type
import Test.Hspec
import Text.RawString.QQ
import Prelude hiding ((*))

[f@(EVar vF), x@(EVar vX), y@(EVar vY), z@(EVar vZ)] = mkEVar <$> ["f", "x", "y", "z"]

mkI = ELit . LInt

spec :: Spec
spec = do
  let fnFXY = MDecl vF ((x, tyBool) +> (y, tyBool) +> EBinOp And x y)

  describe "pMDecl" $ do
    it "parses let declarations" $ do
      parse pMDecl "let f = x" `shouldBe` Right (MDecl vF x)

  describe "pMExec" $ do
    it "parses executable expressions" $ do
      parse pMExec "f(x)" `shouldBe` Right (MExec (f * x))

  describe "pMExpr" $ do
    it "parses mixed module expressions" $ do
      parse pMExpr "let f = \\x:<t> . \\y:<t> . x && y" `shouldBe` Right fnFXY
      parse pMExpr "f(x)(y)" `shouldBe` Right (MExec (f * x * y))
      parse pMExpr "[0 [1][2]]" `shouldBe` Right (MExec (ETree (Node (mkI 0) (Node (mkI 1) Leaf Leaf) (Node (mkI 2) Leaf Leaf))))

  describe "pModule" $ do
    it "parses modules with let declarations" $ do
      let mod0 =
            [r|let f = \x:<n>.\y:<n>. x + y
               let x = 0
               let y = 1
               f(x)(y)|]

      parse pModule mod0
        `shouldBe` Right
          [ MDecl vF ((x, tyInt) +> (y, tyInt) +> EBinOp Add x y),
            MDecl vX $ mkI 0,
            MDecl vY $ mkI 1,
            MExec (f * x * y)
          ]
    it "parses modules with domain declarations" $ do
      let mod0 =
            [r|dom x = {x1,x2,x3}
               dom y = {1,2,3}
               dom z = {True, False}|]

      let vars = EVar . mkVar <$> ["x1", "x2", "x3"]
      let ints = mkI <$> [1, 2, 3]
      let boos = ELit . LBool <$> [True, False]

      let mDom v s = MDecl v (EDom (Dom (TyCon v) (Set.fromList s)))

      parse pModule mod0 `shouldBe` Right [mDom vX vars, mDom vY ints, mDom vZ boos]

    it "parses modules with import declarations" $ do
      let mod0 =
            [r|import (foo,bar) from StdLib.Foo.Bar
               import (baz,bam) from StdLib.Baz.Bam|]

      parse pModule mod0 `shouldBe` Right [MImport [mkVar "foo", mkVar "bar"] ["StdLib", "Foo", "Bar"], MImport [mkVar "baz", mkVar "bam"] ["StdLib", "Baz", "Bam"]]

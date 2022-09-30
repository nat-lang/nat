{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Mean.Syntax.ModuleSpec where

import Mean.Parser (parse)
import Mean.Syntax.Module
import Mean.Syntax.Surface
import Mean.Syntax.Type
import Test.Hspec
import Text.RawString.QQ
import Prelude hiding ((*))

[f@(EVar vF), x@(EVar vX), y@(EVar vY), z] = mkEVar <$> ["f", "x", "y", "z"]

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
    it "parses modules" $ do
      let mod =
            [r|let f = \x:<n>.\y:<n>. x + y
               let x = 0
               let y = 1
               f(x)(y)
            |]
      parse pModule mod
        `shouldBe` Right
          [ MDecl vF ((x, tyInt) +> (y, tyInt) +> EBinOp Add x y),
            MDecl vX $ mkI 0,
            MDecl vY $ mkI 1,
            MExec (f * x * y)
          ]

      let mod =
            [r|let succ = \x:<n> . x + 1
              let x = 0
              let t = [X [succ] [X [succ] [x]]]
              let unit = \e.\b . e
              let FA = \x.\l.\r.\z. tycase (FA l, FA r) of
                  (l',r'):(<A,B>, <A>) -> (l' x) (r' x)
                  (l',r'):(<A>, <A,B>) -> (r' x) (l' x)
                  _ -> unit
            |]

      parse pModule mod
        `shouldBe` Right
          []
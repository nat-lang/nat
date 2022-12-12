{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Nat.Evaluation.ModuleSpec where

import Data.Either (fromRight)
import qualified Data.Map as Map
import Debug.Trace (traceM)
import Nat.Context (Var (..), mkVar)
import Nat.Evaluation.Module
import Nat.Evaluation.Surface (Normalization)
import Nat.Inference
import Nat.Parser
import Nat.Reduction
import Nat.Syntax.Module
import Nat.Syntax.Surface
import Nat.Syntax.Type
import Test.Hspec
import Text.RawString.QQ

mkI = ELit . LInt

mkB = ELit . LBool

spec :: Spec
spec = do
  let p = parse pModule
  let ev = last . fromRight [] . eval

  describe "eval" $ do
    it "evaluates let declarations" $ do
      let (Right mod) =
            p
              [r|let succ = \x. x + 1
                 let four = succ 3|]

          (MDecl (Var v _) e) = ev mod

      v `shouldBe` "four"
      e `shouldBe` mkI 4

    it "evaluates recursive let declarations" $ do
      let (Right mod) =
            p
              [r|let fact = \n. if n <= 1 then 1 else n * (fact (n - 1))
                 fact(10)|]

          expr = ev mod

      expr `shouldBe` MExec (mkI 3628800)

    it "evaluates executables" $ do True `shouldBe` False
    it "evaluates domain declarations" $ do True `shouldBe` False

    it "evaluates trees as functors" $ do
      let (Right mod) =
            p
              [r|let L = \e.\b.e
                 let B = \x.\l.\r.\e.\b. b(x)(l e b)(r e b)

                 let fmap = \f.\t. t(L)(\x. B(f x))
                 let acc = \x.\l.\r. x + l + r

                 let t = [0 [0][0]]
                 (fmap (\x. x + 1) t) == [1 [1][1]]
              |]

      ev mod `shouldBe` MExec (mkB True)

    it "evaluates trees as applicative functors" $ do
      let (Right mod) =
            p
              [r|let L = \e.\b.e
                 let B = \x.\l.\r.\e.\b. b(x)(l e b)(r e b)
                 let pure = \e.[e [e][e]]
                 let fuse = \ft.\xt. xt(\x.\lx.\rx. ft(\f.\lf.\rf. B(f x (fuse lf lx) (fuse rf rx))))
                 let sequence = \t0.\t1. fuse (t0 L) (t1 L)

                 let succ = \x. x + 1
                 let t = sequence (pure succ) [0 [0][0]]

                 t == [1 [1][1]]
              |]

      ev mod `shouldBe` MExec (mkB True)

    it "evaluates traversals over trees" $ do True `shouldBe` False

    it "evaluates type driven composition" $ do
      let (Right mod) =
            p
              [r|let succ = \x. x + 1
                 let tree = [undef [undef [succ] [0]] [succ]]
                 let opFA = \l.\r. tycase (l, r) of
                     (l',r'):(<A>, <A,B>) -> r'(l')
                   | (l',r'):(<A,B>, <A>) -> l'(r')
                   | _ -> undef
                 let guard = \op.\x.\l.\r. tycase x of
                     undef:<undef> -> op l r
                   | _ -> x
                 let compose = \tree.\op. tree(undef)(guard op)
                 compose(tree)(opFA)|]

      ev mod `shouldBe` MExec (mkI 2)

    it "resolves module imports" $ do
      let (Right mod0) = p [r|let succ = \x.x + 1|]
      let (Right mod1) =
            p
              [r|import (succ) from StdLib.Arithmetic
                 succ 0|]

      let ev mods = last . fromRight [] . eval' mods

      ev [NMod ["StdLib", "Arithmetic"] mod0] mod1 `shouldBe` MExec (mkI 1)

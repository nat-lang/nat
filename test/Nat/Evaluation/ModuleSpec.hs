{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Nat.Evaluation.ModuleSpec where

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

  describe "eval" $ do
    it "evaluates let declarations" $ do
      let (Right mod) =
            p
              [r|let succ = \x. x + 1
                 let four = succ 3|]
      case eval mod of
        Left err -> traceM (show err)
        Right mod' -> do
          let (MDecl (Var v _) e) = last mod'
          v `shouldBe` "four"
          e `shouldBe` mkI 4

    it "evaluates recursive let declarations" $ do
      let (Right mod) =
            p
              [r|let fact = \n. if n <= 1 then 1 else n * (fact(n - 1))
                 fact(10)|]
      case eval mod of
        Left err -> traceM (show err)
        Right mod' -> last mod' `shouldBe` MExec (mkI 3628800)

    it "evaluates executables" $ do True `shouldBe` False
    it "evaluates domain declarations" $ do True `shouldBe` False

    it "evaluates functors over trees" $ do
      let (Right mod) =
            p
              [r|let t0 = [0 [0][0]]
                 let t1 = [1 [1][1]]

                 let L = \e.\b.e
                 let B = \x.\l.\r.\e.\b. b(x)(l e b)(r e b)

                 let fmap = \f.\t. t(L)(\x. B(f x))
                 let acc = \x.\l.\r. x + l + r

                 let t0' = fmap (\x. x + 1) t0

                 (t0'(0)(acc)) == (t1(0)(acc))|]

      let (Right mod') = eval mod
      last mod' `shouldBe` MExec (mkB True)

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

      let (Right mod') = eval mod
      last mod' `shouldBe` MExec (mkI 2)

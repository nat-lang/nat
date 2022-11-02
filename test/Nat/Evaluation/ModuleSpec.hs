{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Nat.Evaluation.ModuleSpec where

import qualified Data.Map as Map
import Debug.Trace (traceM)
import Nat.Context (mkVar)
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

spec :: Spec
spec = do
  let ev = eval . parse pModule

  describe "eval" $ do
    it "evaluates let declarations" $ do True `shouldBe` False
    it "evaluates recursive let declarations" $ do True `shouldBe` False
    it "evaluates executables" $ do True `shouldBe` False
    it "evaluates domain declarations" $ do True `shouldBe` False
    it "evaluates struct declarations" $ do
      let dom =
            [r|struct Algebra(( s: Set, ..relations: [Rel<*s>] )) with
                 Closure = \forall r in relations. \forall a in s, b in s. \exists c in s. o(a)(b) == c
                 Uniqueness = 

               struct Group((s, op: (s,s) -> s)) < Algebra where
                 Associativity = 
                 Identity = \exists a in s. \forall b in s. op(a,b) == b
                 Inversion = Forall (a,b) in op. Exists b in dom. 

               struct Monoid(s) = {}
               struct Poset(s)
               struct Lattice(s)

               domain A {a,b,c,d} where
                 (<=) = {(a,b), (b,c), (c,d)} 

               Group(A)
               Monoid(A)
               Poset(A)
               Lattice(A)

               let isClosed(set) = 

               let algebra(set, rels) = 

              |]

    it "evaluates each expr within an accumulated environment" $ do
      let mod =
            [r|let succ = \x. x + 1
               let tree = [undef [undef [succ] [0]] [succ]]
               let opFA = \l.\r. tycase (l, r) of``
                   (l',r'):(<A>, <A,B>) -> r'(l')
                 | (l',r'):(<A,B>, <A>) -> l'(r')
                 | _ -> undef
               let guard = \op.\x.\l.\r. tycase x of
                   undef:<undef> -> op l r
                 | _ -> x
               let compose = \tree.\op. tree(undef)(guard op)
               compose(tree)(opFA)|]

      last (ev mod) `shouldBe` MExec (mkI 2)

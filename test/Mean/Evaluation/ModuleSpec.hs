{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Mean.Evaluation.ModuleSpec where

import qualified Data.Map as Map
import Debug.Trace (traceM)
import Mean.Context (mkVar)
import Mean.Evaluation.Module
import Mean.Evaluation.Surface (Normalization)
import Mean.Inference
import Mean.Parser
import Mean.Reduction
import Mean.Syntax.Module
import Mean.Syntax.Surface
import Mean.Syntax.Type
import Test.Hspec
import Text.RawString.QQ

mkI = ELit . LInt

spec :: Spec
spec = do
  describe "eval" $ do
    it "evaluates each expr within an accumulated environment" $ do
      let mod =
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

      let (Right mod') = parse pModule mod
      let (Right mod'') = eval mod'
      last mod'' `shouldBe` MExec (mkI 2)

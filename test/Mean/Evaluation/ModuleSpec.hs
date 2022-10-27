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

mod0 =
  [r|let succ = \x. x + 1
     let tree = [undef [undef [succ] [0]] [succ]]
     let FA = \l.\r. tycase (l, r) of
        (l',r'):(<A>, <A,B>) -> r'(l')
      | (l',r'):(<A,B>, <A>) -> l'(r')
      | _ -> undef

     let walk = \op.\x.\l.\r. tycase x of
        undef:<undef> -> op l r
      | _ -> x

     let compose = \tree.\op. tree(undef)(walk op)

     compose(tree)(FA)
|]

mkI = ELit . LInt

spec :: Spec
spec = do
  describe "eval" $ do
    it "evaluates each expr within an accumulated environment" $ do
      let (Right mod0') = parse pModule mod0
      let (Right mod0'') = eval mod0'
      last mod0'' `shouldBe` MExec (mkI 2)

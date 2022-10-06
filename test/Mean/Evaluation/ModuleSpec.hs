{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Mean.Evaluation.ModuleSpec where

import qualified Data.Map as Map
import Debug.Trace (traceM)
import Mean.Evaluation.Module
import Mean.Evaluation.Surface (Normalization)
import Mean.Inference
import Mean.Parser
import Mean.Reduction
import Mean.Syntax.Module
import Mean.Syntax.Surface
import Mean.Syntax.Type
import Mean.Var (mkVar)
import Test.Hspec
import Text.RawString.QQ

-- λeb . e
-- λxlreb . b(x)(l e b)(r e b)
-- λeλb[b(0)(b(0)(b(succ)(e)(e))(b(0)(e)(e)))(b(succ)(e)(e))]
mod0 =
  [r|let succ = \x:<n> . x + 1
     let tree = [0 [0 [succ] [0]] [succ]]
     let unit = \e.\b. e
     letrec FA = \x.\l.\r.\z. tycase (FA l, FA r) of
          (l',r'):(<A,B>, <A>) -> l'(r')
        | (l',r'):(<A>, <A,B>) -> r'(l')
        | _ -> x

     let compose = \t.\op. t(unit)(op)

     compose(tree)(FA)(0)
|]

mkI = ELit . LInt

spec :: Spec
spec = do
  describe "eval" $ do
    it "evaluates each expr within an accumulated environment" $ do
      let (Right mod0') = parse pModule mod0
      traceM (show mod0')
      case eval mod0' of
        Left e -> traceM (show e)
        Right mod0'' -> last mod0'' `shouldBe` MExec (mkEVar "z" ~> mkI 2)
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Mean.Evaluation.ModuleSpec where

import Debug.Trace (traceM)
import Mean.Evaluation.Module
import Mean.Parser
import Mean.Syntax.Module
import Mean.Syntax.Surface
import Mean.Syntax.Type
import Test.Hspec
import Text.RawString.QQ

-- λeb . e
-- λxlreb . b(x)(l e b)(r e b)
mod0 =
  [r|let succ = \x:<n> . x + 1
     let tree = [X [X [succ] [0]] [succ]]
     let unit = \e.\b. e
     letrec FA = \x.\l.\r.\z. tycase (FA l, FA r) of
          (l',r'):(<A,B>, <A>) -> (l' x) (r' x)
        | (l',r'):(<A>, <A,B>) -> (r' x) (l' x)
        | _ -> unit

     let compose = \t.\op. t(unit)(op)

     compose(tree)(FA)
|]

p = parse pModule

mkI = ELit . LInt

spec :: Spec
spec = do
  describe "eval" $ do
    it "evaluates each expr within an accumulated environment" $ do
      let (Right mod0') = parse pModule mod0
      case eval mod0' of
        Left e -> traceM (show e)
        Right mod0'' -> last mod0'' `shouldBe` MExec (mkEVar "z" ~> mkI 2)
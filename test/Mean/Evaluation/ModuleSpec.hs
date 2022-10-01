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
let x = 0
let t = [X [succ] [X [succ] [x]]]
let unit = \e.\b . e
let FA = \x.\l.\r.\z. tycase (FA l, FA r) of
                        (l',r'):(<A,B>, <A>) -> (l' x) (r' x)
                        (l',r'):(<A>, <A,B>) -> (r' x) (l' x)
                        _ -> unit

t(unit)(FA)
|]

p = parse pModule

spec :: Spec
spec = do
  describe "eval" $ do
    it "evaluates each expr within an accumulated environment" $ do
      case p mod0 of
        Left e -> traceM (show e)
        Right m0 -> do
          let (Right es) = eval m0
          traceM ("\n=>" ++ show (last es))
          -- e `shouldBe` Right []
          True `shouldBe` False
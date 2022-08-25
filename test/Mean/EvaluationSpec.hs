module Mean.EvaluationSpec where

import Mean.Parser
import Mean.Syntax
import Mean.Evaluation
import Mean.Viz
import Test.Hspec

spec :: Spec
spec = do
  describe "eval" $ do
    let p = parse pExpr

    it "reduces arithmetic expressions" $ do
      let zero = p "\\f\\x . x"
      let one = p "\\f\\x . f x"
      let two = p "\\f\\x . f(f x)"
      let three = p "\\f\\x . f(f(f x))"

      let succ = p "\\n\\f\\x . f(n f x)"

      let add = p "\\m\\n . m succ n"
      let add' = p "\\m\\n\\f\\x . m f (n f x)"

      let mul = p "\\m\\n . m(add n)0"
      let exp = p "\\m\\n . m(mul n)1"
    
    it "reduces boolean expressions" $ do
      let true = \x\y . x
      let false = \x\y . y

      let if = \b\t\f . b(t)(f)

      let and = \x\y . if(x)(y)(false)
      let or = \x\y . if(x)(true)(y)

      let not = \x\y . if(x)(true)(false)
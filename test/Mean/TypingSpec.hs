module Mean.TypingSpec where

import Mean.Parser
import Mean.Syntax
import Mean.TypeEnv
import Mean.Typing
import Mean.Viz
import Test.Hspec

spec :: Spec
spec = do
  describe "inferExpr" $ do
    it "types literal integers" $ do
      inferExpr empty (ELit $ LInt 0) `shouldBe` Right (Forall [] tyInt)
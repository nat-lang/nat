module Mean.TypeSpec where

import Mean.Parser
import Mean.Syntax
import Mean.Type
import Mean.TypeEnv
import Mean.Viz
import Test.Hspec

spec :: Spec
spec = do
  describe "inferExpr" $ do
    it "types literal integers" $ do
      inferExpr empty (ELit $ LInt 0) `shouldBe` Right (Forall [] tyInt)
    it "types literal booleans" $ do
      inferExpr empty (ELit $ LBool True) `shouldBe` Right (Forall [] tyBool)

    -- it "types variables" $ do

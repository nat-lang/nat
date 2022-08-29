module Mean.Module.ParserSpec where  
  
import Mean.Module.Parser
import Mean.Common.Parser (parse)
import Test.Hspec

spec :: Spec
spec = do
  describe "pLet" $ do
    it "parses let declarations" $ do
      parse pLet "let foo = bar" `shouldBe` Right (Let "foo" (mkEVar "bar"))

  describe "pModule" $ do
    it "parses modules" $ do
      parse pModule "let foo = bar \n let bar = foo" `shouldBe` Right [Let "foo" (mkEVar "bar"), Let "bar" (mkEVar "foo")]
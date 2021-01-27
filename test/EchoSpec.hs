module EchoSpec where

import Echo (eval)
import Test.Hspec

spec :: Spec
spec = do
  describe "Echo evaluation" $ do
    it "should return an empty string when empty string is passed" $ do
      eval "" `shouldBe` ""

    it "should return 'a' when 'a' passed in" $ do
      eval "a" `shouldBe` "a"

module StringCalcSpec where

import           Test.Hspec

import           StringCalc (eval)

spec :: Spec
spec = do
  describe "String calculator" $ do
    describe "Simple expressions" $ do
      it "should return 1 when 1 passed in" $ do
        eval "1" `shouldBe` "1"

      it "should return 'Invalid input' on empty string" $ do
        eval "" `shouldBe` "Invalid input"

    describe "single binary expressions" $ do
      it "should return 2 when '1+1' passed in" $ do
        eval "1+1" `shouldBe` "2"

      it "should return 4 when '3+1' passed in" $ do
        eval "3+1" `shouldBe` "4"

      it "should return 2 when '3-1' passed in" $ do
        eval "3-1" `shouldBe` "2"

      it "should return 2 when '3-1' passed in" $ do
        eval "3-1" `shouldBe` "2"

      it "should return 4 when '2*2' passed in" $ do
        eval "2*2" `shouldBe` "4"

      it "should return 2 when '4/2' passed in" $ do
        eval "4/2" `shouldBe` "2"

    describe "unary expressions" $ do
      it "should return -1 when '-1' passed in" $ do
        eval "-1" `shouldBe` "-1"

    describe "complex binary expressions" $ do
      it "should return 7 when '3+2*2' passed in" $ do
        eval "3+2*2" `shouldBe` "7"

      it "should return 10 when '(3+2)*2' passed in" $ do
        eval "(3+2)*2" `shouldBe` "10"

      it "should ignore spaces" $ do
        eval " (      3+ 2   ) * 2   " `shouldBe` "10"

      it "should consume the entire input" $ do
        eval " 2l" `shouldBe` "Invalid input"

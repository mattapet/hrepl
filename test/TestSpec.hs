module TestSpec where

import           Data.Map.Strict as Map
import           Test.Hspec

countC :: String -> Map Char Int
countC ""     = empty
countC (x:xs) = insertWith (+) x 1 $ countC xs

spec :: Spec
spec = do
  describe "character counting" $ do
    it "should return an empty map for an empty string" $ do
      countC "" `shouldBe` Map.empty

    it "should return [('a', 1)] map for an 'a' string" $ do
      countC "a" `shouldBe` fromList [('a', 1)]

    it "should return [('b', 1)] map for an 'a' string" $ do
      countC "b" `shouldBe` fromList [('b', 1)]

    it "should return [('a', 1), ('b', 1)] map for an 'aa' string" $ do
      countC "ab" `shouldBe` fromList [('a', 1), ('b', 1)]

    it "should return [('a', 2)] map for an 'aa' string" $ do
      countC "aa" `shouldBe` fromList [('a', 2)]

    it "should return [('g', 2), ('o', 2), ('l', 1), ('e', 1)] map for an 'google' string" $ do
      countC "google" `shouldBe` fromList [('g', 2), ('o', 2), ('l', 1), ('e', 1)]

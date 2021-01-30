module StringCalcSpec where

import           Test.Hspec

import           Control.Monad
import           StringCalc
import           Text.Printf

testSuites =
  [ ("simple expressions",         [ ("1", "1")
                                   , ("", "Invalid input")
                                   , ("2l", "Invalid input")
                                   ])
  , ("single binary expressions",  [ ("1+1", "2")
                                   , ("3+1", "4")
                                   , ("3-1", "2")
                                   , ("3-1", "2")
                                   , ("2*2", "4")
                                   , ("4/2", "2")
                                   ])
  , ("unary expressions",          [ ("-1", "-1") ])
  , ("complex binary expressions", [ ("-1", "-1")
                                   , ("3+2*2", "7")
                                   , ("(3+2)*2", "10")
                                   , (" (      3+ 2   ) * 2   ", "10")
                                   ])
  ]

spec :: Spec
spec = do
  describe "String calculator" $ do
    forM_ testSuites $ \ (suiteName, datasets) ->
      describe suiteName $ forM_ datasets $ \ (input, result) ->
        it (printf "should return %s when '%s' passed in" result input) $ do
          eval input `shouldBe` result

    describe "Expression evaluation" $ do
      it "should return 1 for Int 1" $ do
        evalE (IntE 1) `shouldBe` 1

      it "should return 4 for AddE (IntE 1) (IntE 3)" $ do
        evalE (AddE (IntE 1) (IntE 3)) `shouldBe` 4

      it "should return 12 for MulE (IntE 4) (IntE 3)" $ do
        evalE (MulE (IntE 4) (IntE 3)) `shouldBe` 12


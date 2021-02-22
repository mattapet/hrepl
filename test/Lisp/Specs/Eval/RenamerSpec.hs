module Lisp.Specs.Eval.RenamerSpec
  ( spec
  ) where

import           Lisp.Core
import           Lisp.Renamer                   ( rename )
import           Test.Hspec

spec :: Spec
spec = do
  describe "Renamer" $ do
    it "should rename function arguments" $ do
      let input =
            List
              [ Identifier "defun"
              , Identifier "f"
              , List [Identifier "a"]
              , Identifier "a"
              ]
      let output = Right $ List
            [ Identifier "defun"
            , Identifier "f"
            , List [Identifier "a_$1"]
            , Identifier "a_$1"
            ]
      rename input `shouldBe` output

    it "should rename lambda arguments" $ do
      let input =
            List [Identifier "lambda", List [Identifier "a"], Identifier "a"]
      let
        output =
          Right $ List
            [Identifier "lambda", List [Identifier "a_$1"], Identifier "a_$1"]
      rename input `shouldBe` output

    it "should rename let binding variables" $ do
      let input = List
            [ Identifier "let"
            , List [Identifier "a", List [Identifier "b", Number 1]]
            , List [Identifier "+", Identifier "a", Identifier "b"]
            ]
      let output = Right $ List
            [ Identifier "let"
            , List [Identifier "a_$1", List [Identifier "b_$1", Number 1]]
            , List [Identifier "+", Identifier "a_$1", Identifier "b_$1"]
            ]
      rename input `shouldBe` output

    it "should rename lambda arguments recursively" $ do
      let input = List
            [ Identifier "lambda"
            , List [Identifier "a"]
            , List
              [ List
                [Identifier "lambda", List [Identifier "a"], Identifier "a"]
              , Identifier "a"
              ]
            ]
      let
        output = Right $ List
          [ Identifier "lambda"
          , List [Identifier "a_$1"]
          , List
            [ List
              [Identifier "lambda", List [Identifier "a_$2"], Identifier "a_$2"]
            , Identifier "a_$1"
            ]
          ]
      rename input `shouldBe` output
      let input = List
            [ Identifier "lambda"
            , List [Identifier "a"]
            , List
              [ Identifier "let"
              , List [List [Identifier "b", Identifier "a"]]
              , List [Identifier "+", Identifier "b", Identifier "a"]
              ]
            ]
      let output = Right $ List
            [ Identifier "lambda"
            , List [Identifier "a_$1"]
            , List
              [ Identifier "let"
              , List [List [Identifier "b_$1", Identifier "a_$1"]]
              , List [Identifier "+", Identifier "b_$1", Identifier "a_$1"]
              ]
            ]
      rename input `shouldBe` output

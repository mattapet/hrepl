module Lisp.Specs.CoreSpec
  ( spec
  ) where

import           Control.Monad                  ( forM_ )
import           Lisp.Core
import           Test.Hspec
import           Text.Printf                    ( printf )

spec :: Spec
spec = do
  describe "Converting Exprs to strings" $ do
    let
      testSuites =
        [ (Nil           , "nil")
        , (Boolean True  , "true")
        , (Boolean False , "false")
        , (Number 1234   , "1234")
        , (Identifier "n", "n")
        , (Application (Identifier "+") [Number 1, Number 2], "(+ 1 2)")
        , ( FuncDef
            "f"
            ["a", "b"]
            (Application (Identifier "+") [Identifier "a", Identifier "b"])
          , "(defun f [a b] (+ a b))"
          )
        ]
    forM_ testSuites $ \(input, result) ->
      it (printf "should convert %s to %s" (show input) (show result)) $ do
        format input `shouldBe` result

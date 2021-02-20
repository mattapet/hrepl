module Lisp.Specs.ParserSpec
  ( spec
  ) where

import           Control.Monad                  ( forM_ )
import           Lisp.Core
import           Lisp.Parser                    ( parseExpr )
import           Text.Printf                    ( printf )

import           Test.Hspec

spec :: Spec
spec = do
  describe "atoms" $ do
    let testSuites =
          [ ("nil"             , Nil)
          , ("()"              , Nil)
          , ("true"            , Boolean True)
          , ("false"           , Boolean False)
          , ("1"               , Number 1)
          , ("-1"              , Number (-1))
          , ("test-identifier" , Identifier "test-identifier")
          , ("test-identifier2", Identifier "test-identifier2")
          ]
    forM_ testSuites $ \(input, result) ->
      it (printf "should parse %s to %s" (show input) (show result)) $ do
        parseExpr input `shouldBe` Right result

  describe "operators" $ do
    let testSuites = ["+", "-", "*", "/", "%", "=", "<", ">"]
    forM_ testSuites $ \operator ->
      it (printf "should parse operator %s" (show operator)) $ do
        parseExpr operator `shouldBe` Right (Identifier operator)

  describe "applications" $ do
    let
      testSuites =
        [ ("(a)"    , Application (Identifier "a") [])
        , ("(+ 1 2)", Application (Identifier "+") [Number 1, Number 2])
        , ( "(+ 1 (+ 1 2))"
          , Application
            (Identifier "+")
            [Number 1, Application (Identifier "+") [Number 1, Number 2]]
          )
        ]

    forM_ testSuites $ \(input, result) ->
      it (printf "should parse %s to %s" (show input) (show result)) $ do
        parseExpr input `shouldBe` Right result


  describe "functions" $ do
    let testSuites =
          [ ( "(defun f [a b] (+ a b))"
            , FuncDef
              "f"
              ["a", "b"]
              (Application (Identifier "+") [Identifier "a", Identifier "b"])
            )
          ]
    forM_ testSuites $ \(input, result) ->
      it (printf "should parse %s to %s" (show input) (show result)) $ do
        parseExpr input `shouldBe` Right result

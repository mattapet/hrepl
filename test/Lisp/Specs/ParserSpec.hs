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
    let
      testSuites =
        [ ("nil"                       , List [List []])
        , ("()"                        , List [List []])
        , ("true"                      , List [Boolean True])
        , ("false"                     , List [Boolean False])
        , ("1"                         , List [Number 1])
        , ("-1"                        , List [Number (-1)])
        , ("test-identifier"           , List [Identifier "test-identifier"])
        , ("test-identifier2"          , List [Identifier "test-identifier2"])
        , ("\"\""                      , List [StringLit ""])
        , ("\"str-literal\""           , List [StringLit "str-literal"])
        , ("\"escaped-\\\"string\\\"\"", List [StringLit "escaped-\"string\""])
        , ("'(1 2 3 4 5)", List [Quote $ List $ Number <$> [1 .. 5]])
        ]
    forM_ testSuites $ \(input, result) ->
      it (printf "should parse %s to %s" (show input) (show result)) $ do
        parseExpr input `shouldBe` Right result

  describe "operators" $ do
    let testSuites = ["+", "-", "*", "/", "%", "=", "<", ">"]
    forM_ testSuites $ \operator ->
      it (printf "should parse operator %s" (show operator)) $ do
        parseExpr operator `shouldBe` Right (List [Identifier operator])

  describe "applications" $ do
    let
      testSuites =
        [ ("(a)"    , List [List [Identifier "a"]])
        , ("(+ 1 2)", List [List [Identifier "+", Number 1, Number 2]])
        , ( "(+ 1 (+ 1 2))"
          , List
            [ List
                [ Identifier "+"
                , Number 1
                , List [Identifier "+", Number 1, Number 2]
                ]
            ]
          )
        ]

    forM_ testSuites $ \(input, result) ->
      it (printf "should parse %s to %s" (show input) (show result)) $ do
        parseExpr input `shouldBe` Right result


  describe "functions" $ do
    let
      testSuites =
        [ ( "(defun f (a b) (+ a b))"
          , List
            [ List
                [ Identifier "defun"
                , Identifier "f"
                , List [Identifier "a", Identifier "b"]
                , List [Identifier "+", Identifier "a", Identifier "b"]
                ]
            ]
          )
        , ( "(defun f (a) a)\n(defun g (b) b)"
          , List
            [ List
              [ Identifier "defun"
              , Identifier "f"
              , List [Identifier "a"]
              , Identifier "a"
              ]
            , List
              [ Identifier "defun"
              , Identifier "g"
              , List [Identifier "b"]
              , Identifier "b"
              ]
            ]
          )
        ]
    forM_ testSuites $ \(input, result) ->
      it (printf "should parse %s to %s" (show input) (show result)) $ do
        parseExpr input `shouldBe` Right result

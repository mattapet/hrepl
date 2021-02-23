module Lisp.Specs.Eval.BuiltinSpec
  ( spec
  ) where

import           Control.Monad                  ( forM_ )
import           Data.StateT
import           Lisp.Core
import           Lisp.Eval                      ( eval )
import           Lisp.Lib
import           Test.Hspec
import           Text.Printf                    ( printf )

spec :: Spec
spec = do
  describe "atom evaluation" $ do
    let testSuite =
          [ ([]               , List []       , Right $ List [])
          , ([]               , Boolean True  , Right $ Boolean True)
          , ([]               , Number 1      , Right $ Number 1)
          , ([]               , StringLit "x" , Right $ StringLit "x")
          , ([], Identifier "x", Left "Found unbound variable 'x'")
          , ([("x", Number 1)], Identifier "x", Right $ Number 1)
          , ( []
            , Quote $ List [Number 1, Number 2]
            , Right $ Quote $ List [Number 1, Number 2]
            )
          ]
    forM_ testSuite $ \(env, input, result) ->
      it (printf "should evaluate %s to %s" (show input) (show result)) $ do
        (fst <$> runResult env (eval input)) `shouldBe` result

  let makeApp name args = List (Identifier name : args)

  describe "functions" $ do
    let defun name args body =
          List
            [ Identifier "defun"
            , Identifier name
            , List (Identifier <$> args)
            , body
            ]
    let
      testSuit =
        [ ( []
          , defun "id" ["a"] (Identifier "a")
          , Right (List [], [("id", Func [] ["a"] (List [Identifier "a"]))])
          )
        , ( [("id", Func [] ["a"] (Identifier "a"))]
          , makeApp "id" [Number 1]
          , Right (Number 1, [("id", Func [] ["a"] (Identifier "a"))])
          )
        , ( [ ( "add"
              , Func []
                     ["a", "b"]
                     (makeApp "+" [Identifier "a", Identifier "b"])
              )
            ]
          , makeApp "add" [Number 1, Number 2]
          , Right
            ( Number 3
            , [ ( "add"
                , Func []
                       ["a", "b"]
                       (makeApp "+" [Identifier "a", Identifier "b"])
                )
              ]
            )
          )
        , ( [ ( "add"
              , Func []
                     ["a", "b"]
                     (makeApp "+" [Identifier "a", Identifier "b"])
              )
            ]
          , makeApp "add" [Number 1, Number 2, Number 3]
          , Left "Invalid number of arguments provided. Expected 2, received 3"
          )
        , ( []
          , List [Identifier "defun"]
          , Left
            "Invalid function definition. Function expects function name, list of arguments and body"
          )
        , ( []
          , List [Identifier "defun", Identifier "f", List [Number 1], List []]
          , Left
            "Invalid function definition. Argument name must be an identifier"
          )
        ]
    forM_ testSuit $ \(env, input, result) ->
      it (printf "should evaluate %s to %s" (show input) (show result)) $ do
        runResult env (eval input) `shouldBe` result

  describe "function passing" $ do
    let
      env =
        [ ( "flip"
          , Func
            []
            ["f_$1"]
            (List
              [ List
                  [ Identifier "lambda"
                  , List [Identifier "x_$1", Identifier "y_$1"]
                  , List
                    [ List
                        [ Identifier "f_$1"
                        , Identifier "y_$1"
                        , Identifier "x_$1"
                        ]
                    ]
                  ]
              ]
            )
          )
        , ( "ff"
          , Func
            []
            ["a", "b"]
            (makeApp "cons" [Identifier "a", makeApp "cons" [Identifier "b"]])
          )
        ]
    let input  = List [makeApp "flip" [Identifier "ff"], Number 1, Number 2]
    let result = Right (Quote $ List [Number 2, Number 1], env)
    it "should flip function arguments" $ do
      runResult env (eval input) `shouldBe` result

  describe "let binding" $ do
    let testSuite =
          [ ( []
            , makeApp "let" [List [Identifier "a"], Identifier "a"]
            , Right (List [], [])
            )
          , ( []
            , makeApp
              "let"
              [ List [List [Identifier "a", Number 2]]
              , List [Identifier "+", Identifier "a", Identifier "a"]
              ]
            , Right (Number 4, [])
            )
          , ( []
            , makeApp
              "let"
              [ List [List [Number 1, Number 2]]
              , List [Identifier "+", Identifier "a", Identifier "a"]
              ]
            , Left "Invalid 'let' binding symbol."
            )
          ]
    forM_ testSuite $ \(env, input, result) ->
      it (printf "should evaluate %s to %s" (show input) (show result)) $ do
        runResult env (eval input) `shouldBe` result

  describe "conditionals" $ do
    let
      testSuite =
        [ ( []
          , makeApp "if" [Boolean True, Number 1, Number 2]
          , Right $ Number 1
          )
        , ( []
          , makeApp "if" [Boolean False, Number 1, Number 2]
          , Right $ Number 2
          )
        , ( []
          , makeApp "if" [Number 1, Number 1, Number 2]
          , Left "Invalid argument type. Condition must evaluate to Boolean"
          )
        , ( []
          , makeApp "if" [Number 1, Number 1]
          , Left
            "Invalid number of arguments. 'if' expects exactly three arguments"
          )
        ]
    forM_ testSuite $ \(env, input, result) ->
      it (printf "should evaluate %s to %s" (show input) (show result)) $ do
        (fst <$> runResult env (eval input)) `shouldBe` result

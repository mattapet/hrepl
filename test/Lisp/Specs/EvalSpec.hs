module Lisp.Specs.EvalSpec where

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
          , ([], Identifier "x", Left "Found unbound variable 'x'")
          , ([("x", Number 1)], Identifier "x", Right $ Number 1)
          ]
    forM_ testSuite $ \(env, input, result) ->
      it (printf "should evaluate %s to %s" (show input) (show result)) $ do
        (fst <$> runResult env (eval input)) `shouldBe` result

  let makeApp name args = List (Identifier name : args)
  describe "primitives application" $ do
    let
      testSuites =
        [ ( "atom applications"
          , [ ([], List []                 , Right $ List [])
            , ([], List [List []]          , Right $ List [])
            , ([], List [Number 2]         , Right $ Number 2)
            , ([], List [List [], Number 2], Right $ Number 2)
            ]
          )
        , ( "numeric operations"
          , [ ([], makeApp "+" [Number 1, Number 2]          , Right $ Number 3)
            , ([], makeApp "-" [Number 4, Number 2]          , Right $ Number 2)
            , ([], makeApp "*" [Number 2, Number 2, Number 2], Right $ Number 8)
            , ([], makeApp "/" [Number 4, Number 2, Number 2], Right $ Number 1)
            , ([], makeApp "mod" [Number 7, Number 4]        , Right $ Number 3)
            ]
          )
        , ( "predicates"
          , [ ( []
              , makeApp "eq" [Boolean True, Boolean True]
              , Right $ Boolean True
              )
            , ( []
              , makeApp "eq" [Boolean True, Boolean False]
              , Right $ Boolean False
              )
            , ([], makeApp "eq" [Number 1, Number 1], Right $ Boolean True)
            , ( []
              , makeApp "eq" [Boolean True]
              , Left
                "Invalid number of arguments. 'eq' expects exactly two arguments"
              )
            , ( []
              , makeApp "eq" [Boolean True, Boolean True, List []]
              , Left
                "Invalid number of arguments. 'eq' expects exactly two arguments"
              )
            , ([], makeApp "null" [List []] , Right $ Boolean True)
            , ([], makeApp "null" [Number 1], Right $ Boolean False)
            , ( []
              , makeApp "null" []
              , Left
                "Invalid number of arguments. 'null' expects exactly one argument"
              )
            ]
          )
        , ( "boolean operations"
          , [ ([], makeApp "not" [Boolean True] , Right $ Boolean False)
            , ([], makeApp "not" [Boolean False], Right $ Boolean True)
            , ( []
              , makeApp "not" [Number 1]
              , Left "Invalid argument type. 'not' expects boolean argument"
              )
            , ( []
              , makeApp "not" [Number 1, Boolean False]
              , Left
                "Invalid number of arguments. 'not' expects exactly one argument"
              )
            , ([], makeApp "=" [Number 1, Number 1], Right $ Boolean True)
            , ( []
              , makeApp "=" [Number 1, Number 1, Number 1]
              , Right $ Boolean True
              )
            , ( []
              , makeApp "=" []
              , Left
                "Invalid number of arguments. Operator '=' expects at least one argument"
              )
            , ([], makeApp "<" [Number 1, Number 2] , Right $ Boolean True)
            , ([], makeApp "<" [Number 1, Number 1] , Right $ Boolean False)
            , ([], makeApp "<" [Number 2, Number 1] , Right $ Boolean False)
            , ([], makeApp "<=" [Number 1, Number 2], Right $ Boolean True)
            , ([], makeApp "<=" [Number 1, Number 1], Right $ Boolean True)
            , ([], makeApp "<=" [Number 2, Number 1], Right $ Boolean False)
            , ([], makeApp ">" [Number 1, Number 2] , Right $ Boolean False)
            , ([], makeApp ">" [Number 1, Number 1] , Right $ Boolean False)
            , ([], makeApp ">" [Number 2, Number 1] , Right $ Boolean True)
            , ([], makeApp ">=" [Number 1, Number 2], Right $ Boolean False)
            , ([], makeApp ">=" [Number 1, Number 1], Right $ Boolean True)
            , ([], makeApp ">=" [Number 2, Number 1], Right $ Boolean True)
            ]
          )
        , ( "conditionals"
          , [ ( []
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
          )
        ]
    forM_ testSuites $ \(desc, testSuite) -> describe desc $ do
      forM_ testSuite $ \(env, input, result) ->
        it (printf "should evaluate %s to %s" (show input) (show result)) $ do
          (fst <$> runResult env (eval input)) `shouldBe` result

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
          , Right
            ( Func [] ["a"] (List [Identifier "a"])
            , [("id", Func [] ["a"] (List [Identifier "a"]))]
            )
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

  describe "io" $ do
    let testSuite =
          [ ( []
            , (("", "")           , makeApp "write" [Number 1])
            , (Right (List [], []), ("", "1"))
            )
          , ( []
            , (("", "")           , makeApp "write-line" [Number 1])
            , (Right (List [], []), ("", "1\n"))
            )
          , ( []
            , (("", "")           , makeApp "write-line" [Boolean True])
            , (Right (List [], []), ("", "true\n"))
            )
          ]

    forM_ testSuite $ \(env, (s, input), result) ->
      it (printf "should eval %s to %s" (show (s, input)) (show result)) $ do
        runResultState s env (eval input) `shouldBe` result

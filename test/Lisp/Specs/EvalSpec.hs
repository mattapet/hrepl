module Lisp.Specs.EvalSpec where

import           Control.Monad                  ( forM_ )
import           Lisp.Core
import           Lisp.Eval                      ( eval )
import           Test.Hspec
import           Text.Printf                    ( printf )

spec :: Spec
spec = do
  describe "atom evaluation" $ do
    let testSuite =
          [ ([]               , Nil           , Right Nil)
          , ([]               , Boolean True  , Right $ Boolean True)
          , ([]               , Number 1      , Right $ Number 1)
          , ([], Identifier "x", Left "Found unbound variable 'x'")
          , ([("x", Number 1)], Identifier "x", Right $ Number 1)
          ]
    forM_ testSuite $ \(env, input, result) ->
      it (printf "should evaluate %s to %s" (show input) (show result)) $ do
        eval env input `shouldBe` result

  describe "primitives application" $ do
    let makeApp name args = Application (Identifier name) args
    let
      testSuites =
        [ ( "numeric operations"
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
              , makeApp "eq" [Boolean True, Boolean True, Nil]
              , Left
                "Invalid number of arguments. 'eq' expects exactly two arguments"
              )
            , ([], makeApp "null" [Nil]     , Right $ Boolean True)
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
          eval env input `shouldBe` result

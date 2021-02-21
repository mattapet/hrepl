module Lisp.Specs.Eval.PrimitivesSpec
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
        , ( "string operations"
          , [ ([], makeApp "length" [StringLit "12345"], Right $ Number 5)
            , ( []
              , makeApp "length" [Number 2]
              , Left
                "Invalid argument type. Function 'length' is applicable only on strings"
              )
            , ( []
              , makeApp "length" []
              , Left
                "Invalid number of arguments. Function 'length' can be applied to only one argument"
              )
            , ( []
              , makeApp "upcase" [StringLit "abcd"]
              , Right $ StringLit "ABCD"
              )
            , ( []
              , makeApp "downcase" [StringLit "ABCD"]
              , Right $ StringLit "abcd"
              )
            ]
          )
        , ( "string operations"
          , [ ( []
              , makeApp "cons" [Number 1, List []]
              , Right $ Quote $ List [Number 1]
              )
            , ( []
              , makeApp "cons" [Number 1, Quote $ List [Number 2]]
              , Right $ Quote $ List [Number 1, Number 2]
              )
            , ([], makeApp "cons" [Number 1], Right $ Quote $ List [Number 1])
            , ( []
              , makeApp "cons" []
              , Left
                "Invalid number of arguments. 'cons' expects one or two arguments"
              )
            , ( []
              , makeApp "car" [Quote $ List [Number 1, Number 2]]
              , Right $ Number 1
              )
            , ([], makeApp "car" [Quote $ List []], Right $ List [])
            , ([], makeApp "car" [List []]        , Right $ List [])
            , ( []
              , makeApp "car" [Number 1]
              , Left
                "Invalid argument type. 'car' operator can only be used on lists"
              )
            , ( []
              , makeApp "car" [List [], List []]
              , Left
                "Invalid number of arguments type. 'car' expects exactly one argument"
              )
            , ( []
              , makeApp "cdr" [Quote $ List [Number 1, Number 2]]
              , Right $ Quote $ List [Number 2]
              )
            , ([], makeApp "cdr" [Quote $ List []], Right $ List [])
            , ([], makeApp "cdr" [List []]        , Right $ List [])
            , ( []
              , makeApp "cdr" [Number 1]
              , Left
                "Invalid argument type. 'cdr' operator can only be used on lists"
              )
            , ( []
              , makeApp "cdr" [List [], List []]
              , Left
                "Invalid number of arguments type. 'cdr' expects exactly one argument"
              )
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
            , ([], makeApp "null" [List []]        , Right $ Boolean True)
            , ([], makeApp "null" [Quote $ List []], Right $ Boolean True)
            , ([], makeApp "null" [Number 1]       , Right $ Boolean False)
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
        ]
    forM_ testSuites $ \(desc, testSuite) -> describe desc $ do
      forM_ testSuite $ \(env, input, result) ->
        it (printf "should evaluate %s to %s" (show input) (show result)) $ do
          (fst <$> runResult env (eval input)) `shouldBe` result

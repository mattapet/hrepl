module Lisp.Specs.Eval.IOPrimitivesSpec
  ( spec
  ) where

import           Control.Monad                  ( forM_ )
import           Lisp.Core
import           Lisp.Eval
import           Lisp.Lib                       ( runResultState )
import           Test.Hspec
import           Text.Printf                    ( printf )

spec :: Spec
spec = do
  describe "io primitives" $ do
    let makeApp name args = List (Identifier name : args)
    let
      testSuite =
        [ ( []
          , (("", "")           , makeApp "write" [Number 1])
          , (Right (List [], []), ("", "1"))
          )
        , ( []
          , (("", "")           , makeApp "write-line" [Number 1])
          , (Right (List [], []), ("", "1\n"))
          )
        , ( [("greeting", StringLit "Hello World!")]
          , (("", ""), makeApp "write-line" [Identifier "greeting"])
          , ( Right (List [], [("greeting", StringLit "Hello World!")])
            , ("", "Hello World!\n")
            )
          )
        , ( []
          , (("", "")           , makeApp "write-line" [Boolean True])
          , (Right (List [], []), ("", "true\n"))
          )
        , ( []
          , (("1", "")                , makeApp "read" [])
          , (Right (StringLit "1", []), ("", ""))
          )
        , ( []
          , (("some-line\nanother-line", "")  , makeApp "read-line" [])
          , (Right (StringLit "some-line", []), ("another-line", ""))
          )
        ]

    forM_ testSuite $ \(env, (s, input), result) ->
      it (printf "should eval %s to %s" (show (s, input)) (show result)) $ do
        runResultState s env (eval input) `shouldBe` result

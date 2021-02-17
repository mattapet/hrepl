module StringCalcSpec where

import           Test.Hspec

import           Control.Monad
import           Data.Map.Strict               as Map
import           StringCalc
import           Text.Printf

testSuites =
  [ ( "simple expressions"
    , [ ((Map.empty, "1") , ("1", Map.empty))
      , ((Map.empty, "")  , ("Invalid input", Map.empty))
      , ((Map.empty, "2l"), ("Invalid input", Map.empty))
      ]
    )
  , ( "single binary expressions"
    , [ ((Map.empty, "1+1"), ("2", Map.empty))
      , ((Map.empty, "3+1"), ("4", Map.empty))
      , ((Map.empty, "3-1"), ("2", Map.empty))
      , ((Map.empty, "3-1"), ("2", Map.empty))
      , ((Map.empty, "2*2"), ("4", Map.empty))
      , ((Map.empty, "4/2"), ("2", Map.empty))
      ]
    )
  , ("unary expressions", [((Map.empty, "-1"), ("-1", Map.empty))])
  , ( "complex binary expressions"
    , [ ((Map.empty, "-1")                     , ("-1", Map.empty))
      , ((Map.empty, "3+2*2")                  , ("7", Map.empty))
      , ((Map.empty, "(3+2)*2")                , ("10", Map.empty))
      , ((Map.empty, " (      3+ 2   ) * 2   "), ("10", Map.empty))
      ]
    )
  , ( "variables"
    , [ ((Map.empty, "let x = 12")      , ("12", fromList [("x", 12)]))
      , ((fromList [("x", 12)], "x / 4"), ("3", fromList [("x", 12)]))
      ]
    )
  ]

spec :: Spec
spec = do
  describe "String calculator" $ do
    forM_ testSuites $ \(suiteName, datasets) ->
      describe suiteName $ forM_ datasets $ \(input, result) ->
        it
            (printf "should return %s when '%s' passed in"
                    (show result)
                    (show input)
            )
          $ do
              uncurry eval input `shouldBe` result


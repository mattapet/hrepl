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
    let testSuites =
          [ (("Nil", "nil")            , (Nil, "nil"))
          , (("Boolean True", "true")  , (Boolean True, "true"))
          , (("Boolean False", "false"), (Boolean False, "false"))
          , (("Number 1234", "1234")   , (Number 1234, "1234"))
          , (("Variable n", "n")       , (Variable "n", "n"))
          , ( ("Application (Variable \"+\") [Number 1, Number 2]", "(+ 1 2)")
            , (Application (Variable "+") [Number 1, Number 2]    , "(+ 1 2)")
            )
          ]
    forM_ testSuites $ \((i, r), (input, result)) ->
      it (printf "should convert %s to '%s'" i r) $ do
        show input `shouldBe` result

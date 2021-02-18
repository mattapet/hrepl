module Lisp.Core where

data Expr =
    Nil
  | Boolean Bool
  | Number Integer
  | Identifier String
  | Application Expr [Expr]
  deriving (Eq, Show)

format :: Expr -> String
format Nil                = "nil"
format (Boolean    True ) = "true"
format (Boolean    False) = "false"
format (Number     x    ) = show x
format (Identifier n    ) = n
format (Application x xs) = "(" ++ unwords (format <$> (x : xs)) ++ ")"

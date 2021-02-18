module Lisp.Core where

data Expr =
    Nil
  | Boolean Bool
  | Number Integer
  | Variable String
  | Application Expr [Expr]

instance Show Expr where
  show Nil                = "nil"
  show (Boolean  True   ) = "true"
  show (Boolean  False  ) = "false"
  show (Number   x      ) = show x
  show (Variable n      ) = n
  show (Application x xs) = "(" ++ unwords (show <$> (x : xs)) ++ ")"

module Lisp.Core where

type Name = String

data Expr =
    Nil
  | Boolean Bool
  | Number Integer
  | Identifier Name
  | Application Expr [Expr]
  | FuncDef Name [Name] Expr
  deriving (Eq, Show)

format :: Expr -> String
format Nil                      = "nil"
format (Boolean    True       ) = "true"
format (Boolean    False      ) = "false"
format (Number     x          ) = show x
format (Identifier n          ) = n
format (Application x xs      ) = "(" ++ unwords (format <$> (x : xs)) ++ ")"
format (FuncDef name args body) = "(defun " ++ name ++ args' ++ body' ++ ")"
  where
    args' = " [" ++ unwords args ++ "] "
    body' = format body

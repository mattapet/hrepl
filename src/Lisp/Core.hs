module Lisp.Core where

import           Data.List                      ( find )

type Name = String

data Expr =
    Nil
  | Boolean Bool
  | Number Integer
  | Identifier Name
  | Application Expr [Expr]
  | FuncDef Name [Name] Expr
  | Func Environment [Name] Expr
  deriving (Eq, Show)

type Environment = [(Name, Expr)]

lookupEnv :: Name -> Environment -> Maybe Expr
lookupEnv name = fmap snd . find ((== name) . fst)

format :: Expr -> String
format Nil                = "nil"
format (Boolean    True ) = "true"
format (Boolean    False) = "false"
format (Number     x    ) = show x
format (Identifier n    ) = n
format (Application x xs) = "(" ++ unwords (format <$> (x : xs)) ++ ")"
format (Func _ args body) = "([anonymous] [" ++ args' ++ body' ++ ")"
  where
    args' = " [" ++ unwords args ++ "]"
    body' = format body
format (FuncDef name args body) = "(defun " ++ name ++ args' ++ body' ++ ")"
  where
    args' = " [" ++ unwords args ++ "] "
    body' = format body

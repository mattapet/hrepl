module Lisp.Core where

type Name = String

data Expr =
    Nil
  | Boolean Bool
  | Number Integer
  | Identifier Name
  | Application Expr [Expr]
  | FuncDef Name [Name] Expr
  | Func Environment [Name] Expr

instance Show Expr where
  show Nil                   = "Nil "
  show (Boolean    x       ) = "Boolean " ++ show x
  show (Number     x       ) = "Number " ++ show x
  show (Identifier x       ) = "Identifier " ++ show x
  show (Application op args) = "Application " ++ show op ++ " " ++ show args
  show (FuncDef name args body) =
    "FuncDef " ++ show name ++ " " ++ show args ++ " " ++ show body
  show (Func _ args body) = "Func " ++ show args ++ " " ++ show body

instance Eq Expr where
  Nil            == Nil            = True
  (Boolean    x) == (Boolean    y) = x == y
  (Number     x) == (Number     y) = x == y
  (Identifier x) == (Identifier y) = x == y
  (Application xOp xArgs) == (Application yOp yArgs) =
    xOp == yOp && xArgs == yArgs
  (FuncDef xName xArgs xBody) == (FuncDef yName yArgs yBody) =
    xName == yName && xArgs == yArgs && xBody == yBody
  (Func _ xArgs xBody) == (Func _ yArgs yBody) =
    xArgs == yArgs && xBody == yBody
  _ == _ = False

type Environment = [(Name, Expr)]

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

module Lisp.Core where

type Name = String

data Expr =
    Boolean Bool
  | Number Integer
  | Identifier Name
  | StringLit String
  | List [Expr]
  | Func Environment [Name] Expr

instance Show Expr where
  show (Boolean    x       ) = "Boolean " ++ show x
  show (Number     x       ) = "Number " ++ show x
  show (Identifier x       ) = "Identifier " ++ show x
  show (StringLit  x       ) = "StringLit " ++ show x
  show (List       contents) = "List " ++ show contents
  show (Func _ args body   ) = "Func " ++ show args ++ " " ++ show body

instance Eq Expr where
  (Boolean    x ) == (Boolean    y ) = x == y
  (Number     x ) == (Number     y ) = x == y
  (Identifier x ) == (Identifier y ) = x == y
  (StringLit  x ) == (StringLit  y ) = x == y
  (List       xs) == (List       ys) = xs == ys
  (Func _ xArgs xBody) == (Func _ yArgs yBody) =
    xArgs == yArgs && xBody == yBody
  _ == _ = False

type Environment = [(Name, Expr)]

formatEnv :: Environment -> String
formatEnv = unlines . (formatEntry <$>)
  where formatEntry (name, value) = "(" ++ name ++ " " ++ format value ++ ")"

format :: Expr -> String
format (Boolean    True ) = "true"
format (Boolean    False) = "false"
format (Number     x    ) = show x
format (Identifier x    ) = x
format (StringLit  x    ) = x
format (List       []   ) = "nil"
format (List       xs   ) = "(" ++ unwords (format <$> xs) ++ ")"
format (Func _ args _   ) = "<λ" ++ args' ++ ">"
  where args' = " [" ++ unwords args ++ "]"

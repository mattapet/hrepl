{-# LANGUAGE FlexibleContexts #-}

module StringCalc (eval, evalE, Expr (..)) where

import           Data.Functor
import           Data.Functor.Identity
import           Text.Parsec

data Expr =
    IntE Int
  | AddE Expr Expr
  | MinE Expr Expr
  | MulE Expr Expr
  | DivE Expr Expr

evalE :: Expr -> Int
evalE (IntE n)       = n
evalE (AddE lhs rhs) = evalE lhs + evalE rhs
evalE (MinE lhs rhs) = evalE lhs - evalE rhs
evalE (MulE lhs rhs) = evalE lhs * evalE rhs
evalE (DivE lhs rhs) = evalE lhs `div` evalE rhs

digits :: ParsecT String u Identity [Char]
digits = many1 digit

integer  :: ParsecT String u Identity Expr
integer  =            IntE . read <$> do spaces *> digits <* spaces
         <|> IntE . negate . read <$> do spaces *> char '-' *> spaces *> digits <* spaces

parens   :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
parens p = char '(' *> p <* char ')'

expr     :: ParsecT String u Identity Expr
expr     = spaces *> term `chainl1` addop <* spaces

term     :: ParsecT String u Identity Expr
term     = spaces *> factor `chainl1` mulop <* spaces

factor   :: ParsecT String u Identity Expr
factor   = spaces *> parens expr <|> integer<* spaces

addop    :: ParsecT String u Identity (Expr -> Expr -> Expr)
addop    =   spaces *> char '+' <* spaces $> AddE
         <|> spaces *> char '-' <* spaces $> MinE

mulop    :: ParsecT String u Identity (Expr -> Expr -> Expr)
mulop    =   spaces *> char '*' <* spaces $> MulE
         <|> spaces *> char '/' <* spaces $> DivE

parseE :: String -> Either ParseError Expr
parseE = parse (expr <* eof) ""

eval :: String -> String
eval a  = either (const "Invalid input") (show . evalE) (parseE a)

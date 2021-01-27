{-# LANGUAGE FlexibleContexts #-}

module StringCalc
  ( eval,
  ) where

import           Data.Functor
import           Data.Functor.Identity
import           Text.Parsec

digits :: ParsecT String u Identity [Char]
digits = many1 digit

integer :: ParsecT String u Identity Integer
integer   =            read <$> do spaces *> digits <* spaces
          <|> negate . read <$> do spaces *> char '-' *> spaces *> digits <* spaces

parens  :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
parens p  = char '(' *> p <* char ')'

expr    :: ParsecT String u Identity Integer
expr      = spaces *> term `chainl1` addop <* spaces

term    :: ParsecT String u Identity Integer
term      = spaces *> factor `chainl1` mulop <* spaces

factor  :: ParsecT String u Identity Integer
factor    = spaces *> parens expr <|> integer <* spaces

addop   :: ParsecT String u Identity (Integer -> Integer -> Integer)
addop     =   spaces *> char '+' <* spaces $> (+)
          <|> spaces *> char '-' <* spaces $> (-)

mulop   :: ParsecT String u Identity (Integer -> Integer -> Integer)
mulop     =   spaces *> char '*' <* spaces $> (*)
          <|> spaces *> char '/' <* spaces $> div

eval :: String -> String
eval a  = either (const "Invalid input") show (parse (expr <* eof) "" a)

module Lisp.Parser
  ( parseExpr
  ) where

import           Data.Functor
import           Data.Functor.Identity
import           Lisp.Core
import           Text.Parsec

integer :: ParsecT String u Identity Expr
integer = Number <$> (positive <|> negative)
  where
    positive = read <$> many1 digit
    negative = negate <$> (char '-' *> positive)

identifier :: ParsecT String u Identity Expr
identifier = Identifier <$> many1 (letter <|> digit <|> oneOf "-+*/%")

atom :: ParsecT String u Identity Expr
atom = foldl1 (<|>) $ try <$> atomParsers
  where
    atomParsers =
      [ string "nil" $> Nil
      , string "()" $> Nil
      , string "true" $> Boolean True
      , string "false" $> Boolean False
      , integer
      , identifier
      ]

application :: ParsecT String u Identity Expr
application = Application <$> try (char '(' *> atom) <*> many expr <* char ')'

expr :: ParsecT String u Identity Expr
expr = spaces *> (atom <|> application) <* spaces

parseExpr :: String -> Either String Expr
parseExpr = mapLeft show . parse expr ""
  where
    mapLeft _ (Right b) = Right b
    mapLeft f (Left  a) = Left $ f a

module Lisp.Parser
  ( parseExpr
  ) where

import           Data.Functor
import           Data.Functor.Identity
import           Lisp.Core
import           Text.Parsec

-- utility parsers

parens :: ParsecT String u Identity a -> ParsecT String u Identity a
parens p = char '(' *> spaces *> p <* spaces <* char ')'

integer :: ParsecT String u Identity Integer
integer = positive <|> negative
  where
    positive = read <$> many1 digit
    negative = negate <$> (char '-' *> positive)

identifier :: ParsecT String u Identity String
identifier = many1 (letter <|> digit <|> oneOf "-+*/%=<>")

-- Ref: https://stackoverflow.com/questions/24106314/parser-for-quoted-string-using-parsec
stringLiteral :: ParsecT String u Identity String
stringLiteral = char '"' *> many character <* char '"'
  where
    character = nonEscape <|> escape
    nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"
    escape    = char '\\' *> oneOf "\\\"0nrvtbf" -- all the characters which can be escaped


-- Expression parsers

atom :: ParsecT String u Identity Expr
atom = foldl1 (<|>) (try <$> atomParsers) -- <?> "Expected atom"
  where
    atomParsers =
      [ string "nil" $> List []
      , string "()" $> List []
      , string "true" $> Boolean True
      , string "false" $> Boolean False
      , Number <$> integer
      , Identifier <$> identifier
      , StringLit <$> stringLiteral
      ]

list :: ParsecT String u Identity Expr
list = parens $ List <$> many1 expr

expr :: ParsecT String u Identity Expr
expr = spaces *> foldl1 (<|>) (try <$> exprParsers) <* spaces
  where exprParsers = [atom, list]

parseExpr :: String -> Either String Expr
parseExpr = mapLeft show . parse topLevelExpr ""
  where
    topLevelExpr = List <$> many1 expr
    mapLeft _ (Right b) = Right b
    mapLeft f (Left  a) = Left $ f a

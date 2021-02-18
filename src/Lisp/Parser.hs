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

brackets :: ParsecT String u Identity a -> ParsecT String u Identity a
brackets p = char '[' *> spaces *> p <* spaces <* char ']'

integer :: ParsecT String u Identity Integer
integer = positive <|> negative
  where
    positive = read <$> many1 digit
    negative = negate <$> (char '-' *> positive)

-- Expression parsers

identifier :: ParsecT String u Identity String
identifier = many1 (letter <|> digit <|> oneOf "-+*/%")

atom :: ParsecT String u Identity Expr
atom = foldl1 (<|>) (try <$> atomParsers) -- <?> "Expected atom"
  where
    atomParsers =
      [ string "nil" $> Nil
      , string "()" $> Nil
      , string "true" $> Boolean True
      , string "false" $> Boolean False
      , Number <$> integer
      , Identifier <$> identifier
      ]

application :: ParsecT String u Identity Expr
application = parens $ Application <$> atom <*> many expr

function :: ParsecT String u Identity Expr
function =
  parens $ FuncDef <$> (string "defun" *> space *> identifier) <*> args <*> expr
  where
    args = spaces *> brackets (many (spaces *> identifier <* spaces)) <* spaces

expr :: ParsecT String u Identity Expr
expr = spaces *> (try atom <|> try application <|> try function) <* spaces

parseExpr :: String -> Either String Expr
parseExpr = mapLeft show . parse expr ""
  where
    mapLeft _ (Right b) = Right b
    mapLeft f (Left  a) = Left $ f a

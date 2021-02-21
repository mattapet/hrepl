{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module StringCalc
  ( eval
  , StateT
  ) where

import           Data.Bifunctor                 ( first )
import           Data.Functor
import           Data.Functor.Identity
import           Data.Map.Strict               as Map
import           Data.StateT
import           Text.Parsec
import           Text.Printf                    ( printf )

data Expr
  = IntE Int
  | AddE Expr Expr
  | MinE Expr Expr
  | MulE Expr Expr
  | DivE Expr Expr
  | LetE String Expr
  | IdE String
  deriving (Show, Eq)

-- Evaluation

type EvalState a = StateT (Map String Int) (Either String) a

evalBinOp :: (Int -> Int -> Int) -> Expr -> Expr -> EvalState Int
evalBinOp op lhs rhs = do
  l <- evalE lhs
  r <- evalE rhs
  return $ l `op` r

evalE :: Expr -> EvalState Int
evalE (IntE n       ) = pure n
evalE (AddE l    r  ) = evalBinOp (+) l r
evalE (MinE l    r  ) = evalBinOp (-) l r
evalE (MulE l    r  ) = evalBinOp (*) l r
evalE (DivE l    r  ) = evalBinOp div l r
evalE (LetE name val) = do
  r <- evalE val
  s <- get
  put (insert name r s) >> return r
evalE (IdE x) = get >>= liftStateT . unwrapValue . (!? x)
  where
    unwrapValue (Just value) = Right value
    unwrapValue Nothing      = Left (printf "Variable '%s' is not defined" x)

-- Parsing

digits :: ParsecT String u Identity String
digits = many1 digit

identifier :: ParsecT String u Identity String
identifier = spaces *> many1 letter <* spaces

integer :: ParsecT String u Identity Expr
integer = IntE . read <$> positive <|> IntE . negate . read <$> negative
  where
    positive = spaces *> digits <* spaces
    negative = spaces *> char '-' *> spaces *> digits <* spaces

parens :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
parens p = char '(' *> p <* char ')'

expr :: ParsecT String u Identity Expr
expr = spaces *> (let' <|> (term `chainl1` addop)) <* spaces

term :: ParsecT String u Identity Expr
term = spaces *> factor `chainl1` mulop <* spaces

factor :: ParsecT String u Identity Expr
factor = spaces *> factor' <* spaces
  where factor' = parens expr <|> (IdE <$> identifier) <|> integer

let' :: ParsecT String u Identity Expr
let' = LetE <$> (spaces *> string "let" *> identifier <* char '=') <*> expr

addop :: ParsecT String u Identity (Expr -> Expr -> Expr)
addop =
  spaces *> char '+' <* spaces $> AddE <|> spaces *> char '-' <* spaces $> MinE

mulop :: ParsecT String u Identity (Expr -> Expr -> Expr)
mulop =
  spaces *> char '*' <* spaces $> MulE <|> spaces *> char '/' <* spaces $> DivE

parseE :: String -> Either ParseError Expr
parseE = parse (expr <* eof) ""

eval :: Map String Int -> String -> (String, Map String Int)
eval s a = either (, s) (first show) $ evalE' $ parseE a
  where
    evalE' :: Either a Expr -> Either String (Int, Map String Int)
    evalE' (Right e) = runStateT (evalE e) s
    evalE' (Left  _) = Left "Invalid input"

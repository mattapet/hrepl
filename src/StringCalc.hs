{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}

module StringCalc (eval) where

import           Data.Bifunctor        (second)
import           Data.Functor
import           Data.Functor.Identity
import           Data.Map.Strict       as Map
import           Text.Parsec
import           Text.Printf           (printf)

data Expr =
    IntE Int
  | AddE Expr Expr
  | MinE Expr Expr
  | MulE Expr Expr
  | DivE Expr Expr
  | LetE String Expr
  | IdE  String
  deriving (Show, Eq)

evalBinOp :: (Int -> Int -> Int) -> Map String Int -> Expr -> Expr -> Either String (Map String Int, Int)
evalBinOp op s lhs rhs = do
  (s', l)  <- evalE s lhs
  (s'', r) <- evalE s' rhs
  return (s'', l `op` r)

evalE :: Map String Int -> Expr -> Either String (Map String Int, Int)
evalE s (IntE n)        = Right (s, n)
evalE s (AddE lhs rhs)  = evalBinOp (+)   s lhs rhs
evalE s (MinE lhs rhs)  = evalBinOp (-)   s lhs rhs
evalE s (MulE lhs rhs)  = evalBinOp (*)   s lhs rhs
evalE s (DivE lhs rhs)  = evalBinOp (div) s lhs rhs
evalE s (LetE name val) = do
                (s', r) <- evalE s val
                return (insert name r s', r)
evalE s (IdE x)         = case s !? x of
            Just value -> Right (s, value)
            Nothing    -> Left (printf "Variable '%s' is not defined" x)

digits :: ParsecT String u Identity String
digits = many1 digit

identifier :: ParsecT String u Identity String
identifier = spaces *> many1 letter <* spaces

integer  :: ParsecT String u Identity Expr
integer  =            IntE . read <$> do                       spaces *> digits <* spaces
         <|> IntE . negate . read <$> do spaces *> char '-' *> spaces *> digits <* spaces

parens   :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
parens p = char '(' *> p <* char ')'

expr     :: ParsecT String u Identity Expr
expr     = spaces *> (do let' <|> (term `chainl1` addop)) <* spaces

term     :: ParsecT String u Identity Expr
term     = spaces *> factor `chainl1` mulop <* spaces

factor   :: ParsecT String u Identity Expr
factor   = spaces *> (do parens expr <|> (IdE <$> identifier) <|> integer) <* spaces

let'    :: ParsecT String u Identity Expr
let'    = LetE <$> (spaces *> string "let" *> identifier <* char '=') <*> expr

addop   :: ParsecT String u Identity (Expr -> Expr -> Expr)
addop   =   spaces *> char '+' <* spaces $> AddE
        <|> spaces *> char '-' <* spaces $> MinE

mulop   :: ParsecT String u Identity (Expr -> Expr -> Expr)
mulop   =   spaces *> char '*' <* spaces $> MulE
        <|> spaces *> char '/' <* spaces $> DivE

parseE  :: String -> Either ParseError Expr
parseE  = parse (expr <* eof) ""

eval :: Map String Int -> String -> (Map String Int, String)
eval s a = either (s,) (second show) $ evalE' $ parseE a
  where
    evalE' (Right e) = evalE s e
    evalE' (Left _)  = Left "Invalid input"

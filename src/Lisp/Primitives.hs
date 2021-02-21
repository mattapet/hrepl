{-# LANGUAGE LambdaCase #-}

module Lisp.Primitives
  ( primitives
  , Primitive
  ) where

import           Data.Char                      ( toLower
                                                , toUpper
                                                )
import           Lisp.Core

type Primitive m = [Expr] -> m (Either String Expr)

primitives :: (Monad m) => [(Name, Primitive m)]
primitives =
  -- Algebraic operations
  [ ("+"  , pure . makeNumberBinOp "+" (+))
  , ("-"  , pure . makeNumberBinOp "-" (-))
  , ("*"  , pure . makeNumberBinOp "*" (*))
  , ("/"  , pure . makeNumberBinOp "/" div)
  , ("mod", pure . makeNumberBinOp "mod" rem)
  -- String operations
  , ( "length"
    , pure . makeStringTransformation "length" (Number . toInteger . length)
    )
  , ( "upcase"
    , pure . makeStringTransformation "length" (StringLit . (toUpper <$>))
    )
  , ( "downcase"
    , pure . makeStringTransformation "length" (StringLit . (toLower <$>))
    )
  -- Boolean operations
  , ("eq"  , pure . eq)
  , ("null", pure . null')
  , ("not" , pure . not')
  , ("="   , pure . makeBooleanBinOp "=" (==))
  , ("<"   , pure . makeBooleanBinOp "<" (<))
  , ("<="  , pure . makeBooleanBinOp "<=" (<=))
  , (">"   , pure . makeBooleanBinOp ">" (>))
  , (">="  , pure . makeBooleanBinOp ">=" (>=))
  ]


eq :: [Expr] -> Either String Expr
eq [lhs, rhs] = Right $ Boolean $ lhs == rhs
eq _ = Left "Invalid number of arguments. 'eq' expects exactly two arguments"

not' :: [Expr] -> Either String Expr
not' [Boolean x] = Right $ Boolean $ not x
not' [_] = Left "Invalid argument type. 'not' expects boolean argument"
not' _ = Left "Invalid number of arguments. 'not' expects exactly one argument"

null' :: [Expr] -> Either String Expr
null' [List []] = Right $ Boolean True
null' [_      ] = Right $ Boolean False
null' _ =
  Left "Invalid number of arguments. 'null' expects exactly one argument"


makeStringTransformation :: Name
                         -> (String -> Expr)
                         -> [Expr]
                         -> Either String Expr
makeStringTransformation _ op [StringLit s] = Right $ op s
makeStringTransformation n _ [_] =
  Left
    $  "Invalid argument type. Function '"
    ++ n
    ++ "' is applicable only on strings"
makeStringTransformation n _ _ =
  Left
    $  "Invalid number of arguments. Function '"
    ++ n
    ++ "' can be applied to only one argument"

makeBooleanBinOp :: Name
                 -> (Integer -> Integer -> Bool)
                 -> [Expr]
                 -> Either String Expr
makeBooleanBinOp name op xs =
  Boolean <$> (traverse (unpackNum name) xs >>= loop)
  where
    loop [] =
      Left
        $  "Invalid number of arguments. Operator '"
        ++ name
        ++ "' expects at least one argument"
    loop [_] = Right True
    loop (x : y : ys) | x `op` y  = loop $ y : ys
                      | otherwise = Right False

makeNumberBinOp :: Name
                -> (Integer -> Integer -> Integer)
                -> [Expr]
                -> Either String Expr
makeNumberBinOp name op (x : xs) = do
  head' <- unpackNum name x
  tail' <- traverse (unpackNum name) xs
  return $ Number $ foldl1 op (head' : tail')
makeNumberBinOp name _ [] =
  Left $ "Operator '" ++ name ++ "' expects at least one argument"

unpackNum :: Name -> Expr -> Either String Integer
unpackNum _ (Number x) = Right x
unpackNum name _ =
  Left $ "Operator '" ++ name ++ "' accepts only number values"

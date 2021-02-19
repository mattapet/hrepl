{-# LANGUAGE LambdaCase #-}

module Lisp.Primitives
  ( primitives
  , lookupPrimitive
  , Primitive
  ) where

import           Data.List                      ( find )
import           Lisp.Core

type Primitive = [Expr] -> Either String Expr

lookupPrimitive :: Name -> Maybe Primitive
lookupPrimitive name = snd <$> find ((== name) . fst) primitives

primitives :: [(Name, Primitive)]
primitives =
  [ ("+"   , makeNumberBinOp "+" (+))
  , ("-"   , makeNumberBinOp "-" (-))
  , ("*"   , makeNumberBinOp "*" (*))
  , ("/"   , makeNumberBinOp "/" div)
  , ("mod" , makeNumberBinOp "mod" rem)
  , ("eq"  , eq)
  , ("null", null')
  , ("not" , not')
  , ("if"  , if')
  , ("="   , makeBooleanBinOp "=" (==))
  , ("<"   , makeBooleanBinOp "<" (<))
  , ("<="  , makeBooleanBinOp "<=" (<=))
  , (">"   , makeBooleanBinOp ">" (>))
  , (">="  , makeBooleanBinOp ">=" (>=))
  ]


eq :: [Expr] -> Either String Expr
eq [lhs, rhs] = Right $ Boolean $ lhs == rhs
eq _ = Left "Invalid number of arguments. 'eq' expects exactly two arguments"

not' :: [Expr] -> Either String Expr
not' [Boolean x] = Right $ Boolean $ not x
not' [_] = Left "Invalid argument type. 'not' expects boolean argument"
not' _ = Left "Invalid number of arguments. 'not' expects exactly one argument"

null' :: [Expr] -> Either String Expr
null' [Nil] = Right $ Boolean True
null' [_  ] = Right $ Boolean False
null' _ =
  Left "Invalid number of arguments. 'null' expects exactly one argument"

if' :: [Expr] -> Either String Expr
if' [Boolean True , then', _    ] = Right then'
if' [Boolean False, _    , else'] = Right else'
if' [_, _, _] =
  Left "Invalid argument type. Condition must evaluate to Boolean"
if' _ =
  Left "Invalid number of arguments. 'if' expects exactly three arguments"

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

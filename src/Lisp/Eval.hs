module Lisp.Eval where

import           Lisp.Core
import           Lisp.Primitives

eval :: Environment -> Expr -> Either String Expr
eval _ val@Nil            = return val
eval _ val@(Boolean    _) = return val
eval _ val@(Number     _) = return val
eval e (    Identifier x) = case lookupEnv x e of
  Just x' -> return x'
  Nothing -> Left $ "Found unbound variable '" ++ x ++ "'"
eval _ (Application (Identifier op) xs) = case lookupPrimitive op of
  Just prim -> prim xs
  _         -> Left $ "Found unbound variable '" ++ op ++ "'"
eval _ _ = undefined

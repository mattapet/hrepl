{-# LANGUAGE LambdaCase #-}

module Lisp.Eval where

import           Control.Applicative
import           Data.StateT

import           Lisp.Core
import           Lisp.Primitives

lookupVariable :: Name
               -> StateT Environment (Either String) (Either Primitive Expr)
lookupVariable n = do
  env <- get
  maybeToState ((Right <$> lookupEnv n env) <|> (Left <$> lookupPrimitive n))
  where
    maybeToState = maybe unboundError return
    unboundError = liftF $ Left $ "Found unbound variable '" ++ n ++ "'"

eval :: Expr -> StateT Environment (Either String) Expr
eval val@Nil            = return val
eval val@(Boolean    _) = return val
eval val@(Number     _) = return val
eval (    Identifier x) = do
  var <- lookupVariable x
  case var of
    Left  _ -> return $ Identifier x
    Right e -> eval e

eval (Application (Identifier op) xs) = do
  var <- lookupVariable op
  case var of
    Left  prim                -> mapM eval xs >>= liftF . prim
    Right (Func e' args body) -> evaluateInContext (e' ++ zip args xs) body
    Right _                   -> undefined
  where
    evaluateInContext :: Environment
                      -> Expr
                      -> StateT Environment (Either String) Expr
    evaluateInContext context content = do
      currentEnv <- get
      result     <- put context >> eval content
      put currentEnv
      return result


eval (FuncDef name args body) = do
  func <- captureFunctionContext
  _    <- bindToVariable func
  return func
  where
    captureFunctionContext = get >>= \env -> return $ Func env args body
    bindToVariable f = get >>= put . (++ [(name, f)])

eval _ = undefined

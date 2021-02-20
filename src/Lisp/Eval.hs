{-# LANGUAGE LambdaCase #-}

module Lisp.Eval
  ( eval
  ) where

import           Control.Applicative
import           Data.StateT

import           Lisp.Core
import           Lisp.Primitives

type Result a = StateT Environment (Either String) a

-- Utility functions

failWith :: String -> Result a
failWith message = liftF $ Left message

-- | Variable lookup retrieves a variable from the entire accessible context.
--   I.e., current environment as right value, or one of the primitive functions
--   as left value.
lookupVariable :: Name -> Result (Either Primitive Expr)
lookupVariable n = do
  env <- get
  maybeToState ((Right <$> lookup n env) <|> (Left <$> lookup n primitives))
  where
    maybeToState = maybe unboundError return
    unboundError = failWith $ "Found unbound variable '" ++ n ++ "'"

evaluateInContext :: Expr -> Environment -> Result Expr
evaluateInContext content context = do
  currentEnv <- get
  result     <- put context >> eval content
  put currentEnv >> return result

-- Evaluation

eval :: Expr -> Result Expr
-- Atoms
eval val@Nil         = return val
eval val@(Boolean _) = return val
eval val@(Number  _) = return val
eval val@Func{}      = return val
eval (Identifier x)  = lookupVariable x >>= unpack
  where
    unpack (Left  _) = return $ Identifier x
    unpack (Right e) = eval e

-- If

eval (Application (Identifier "if") [cond', then', else']) =
  eval cond' >>= \case
    Boolean True -> eval then'
    Boolean False -> eval else'
    _ -> failWith "Invalid argument type. Condition must evaluate to Boolean"
eval (Application (Identifier "if") _) =
  failWith "Invalid number of arguments. 'if' expects exactly three arguments"


-- Applications
eval (Application (Identifier op) xs) = lookupVariable op >>= \case
  Left prim -> do
    args <- traverse eval xs
    liftF $ prim args
  Right e -> eval (Application e xs)

eval (Application (Func e args body) xs) = bindArguments
  >>= evaluateInContext body
  where
    bindArguments | length args == length xs = (++ e) . zip args <$> xs'
                  | otherwise                = failWith invalidNumberOfArgs
    xs' = traverse eval xs
    invalidNumberOfArgs =
      "Invalid number of arguments provided. Expected "
        ++ show (length args)
        ++ ", received "
        ++ show (length xs)

eval (Application _ _       ) = failWith "Type is not callable"

-- Function declaration
eval (FuncDef name args body) = do
  env <- get
  let func = Func env' args body
      env' = (name, func) : env
  put env' >> return func

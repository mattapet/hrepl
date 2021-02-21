{-# LANGUAGE LambdaCase #-}

module Lisp.Eval
  ( eval
  , liftResult
  , liftResultM
  , runResult
  , runResultM
  ) where

import           Control.Applicative
import           Data.ExceptT
import           Data.Functor.Identity
import           Data.StateT

import           Lisp.Core
import           Lisp.Primitives                ( Primitive
                                                , primitives
                                                )

type Result m a = StateT Environment (ExceptT String m) a

liftResult :: Either String a -> Result Identity a
liftResult = liftResultM

liftResultM :: (Monad m) => Either String a -> Result m a
liftResultM = liftS . liftE

runResult :: Result Identity a -> Environment -> Either String (a, Environment)
runResult r = runIdentity . runExceptT . runStateT r

runResultM :: (Monad m)
           => Result m a
           -> Environment
           -> m (Either String (a, Environment))
runResultM r = runExceptT . runStateT r

-- Utility functions

failWith :: (Monad m) => String -> Result m a
failWith = liftS . throwError

-- | Variable lookup retrieves a variable from the entire accessible context.
--   I.e., current environment as right value, or one of the primitive functions
--   as left value.
lookupVariable :: (Monad m) => Name -> Result m (Either (Primitive m) Expr)
lookupVariable n = do
  env <- get
  convertToState ((Right <$> lookup n env) <|> (Left <$> lookup n primitives))
  where
    convertToState = maybe unboundError return
    unboundError   = failWith $ "Found unbound variable '" ++ n ++ "'"

evaluateInContext :: (Monad m) => Expr -> Environment -> Result m Expr
evaluateInContext content context = do
  currentEnv <- get
  result     <- put context >> eval content
  put currentEnv >> return result

-- Evaluation

eval :: (Monad m) => Expr -> Result m Expr
-- Atoms
eval val@Nil         = return val
eval val@(Boolean _) = return val
eval val@(Number  _) = return val
eval val@Func{}      = return val
eval (Identifier x)  = lookupVariable x >>= unpack
  where
    -- There is nothing more we can do with primitive value, so we just return
    -- their identifier back
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
eval (Application (Identifier op) xs) = lookupVariable op >>= eval'
  where
    eval' (Left  prim) = traverse eval xs >>= liftS . prim
    eval' (Right val ) = eval (Application val xs)

eval (Application (Func e args body) xs) = bindArguments
  >>= evaluateInContext body
  where
    bindArguments
      | length args == length xs = (++ e) . zip args <$> traverse eval xs
      | otherwise                = failWith invalidNumberOfArgs
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

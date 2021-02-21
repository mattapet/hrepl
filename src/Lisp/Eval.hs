{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Lisp.Eval
  ( Eval(..)
  , ResultT(..)
  , liftResultT
  , runResultT
  ) where

import           Control.Applicative
import           Data.ExceptT
import           Data.StateT

import           Lisp.Core
import           Lisp.Primitives                ( Primitive
                                                , basicPrimitives
                                                )

newtype ResultT m a = ResultT (StateT Environment (ExceptT String m) a)
  deriving (Monad, Applicative, Functor)

liftResultT :: (Applicative m) => Either String a -> ResultT m a
liftResultT = ResultT . liftStateT . liftExceptT

runResultT :: Environment -> ResultT m a -> m (Either String (a, Environment))
runResultT env (ResultT r) = runExceptT $ runStateT r env

class (Monad m, MonadFail m) => Eval m where
  getEnv :: m Environment
  setEnv :: Environment -> m ()
  primitives' :: [(Name, Primitive m)]

  -- | Variable lookup retrieves a variable from the entire accessible context.
  --   I.e., current environment as right value, or one of the primitive functions
  --   as left value.
  lookupVariable' :: Name -> m (Either (Primitive m) Expr)
  lookupVariable' n = do
    env <- getEnv
    convertToState ((Right <$> lookup n env) <|> (Left <$> lookup n primitives'))
    where
      convertToState = maybe unboundError return
      unboundError   = fail $ "Found unbound variable '" ++ n ++ "'"

  evaluateInContext :: Expr -> Environment -> m Expr
  evaluateInContext content context = do
    currentEnv <- getEnv
    result     <- setEnv context >> eval content
    setEnv currentEnv >> return result

  -- Evaluation

  eval :: Expr -> m Expr

  -- Atoms

  eval val@(List    []) = return val
  eval val@(Boolean _ ) = return val
  eval val@(Number  _ ) = return val
  eval val@Func{}       = return val
  eval (Identifier x)   = lookupVariable' x >>= unpack
    where
      -- There is nothing more we can do with primitive value, so we just return
      -- their identifier back
      unpack (Left  _) = return $ Identifier x
      unpack (Right e) = eval e

  -- If

  eval (List [Identifier "if", cond', then', else']) = eval cond' >>= \case
    Boolean True -> eval then'
    Boolean False -> eval else'
    _ -> fail "Invalid argument type. Condition must evaluate to Boolean"
  eval (List ((Identifier "if") : _)) =
    fail "Invalid number of arguments. 'if' expects exactly three arguments"

  -- Function declaration

  eval (List (Identifier "defun" : Identifier name : List args : body)) = do
    env   <- getEnv
    args' <- unpackArgs args
    let func = Func env' args' (List body)
        env' = (name, func) : env
    setEnv env' >> return func
    where
      unpackArgs = traverse unpackArg
      unpackArg (Identifier n) = return n
      unpackArg _              = fail
        "Invalid function definition. Argument name must be an identifier"

  eval (List ((Identifier "defun") : _)) =
    fail
      "Invalid function definition. Function expects function name, list of arguments and body"


  -- Applications
  eval (List ((Identifier op) : xs)) = lookupVariable' op >>= eval'
    where
      eval' (Left prim) = traverse eval xs >>= prim >>= \case
        Right x -> return x
        Left  e -> fail e
      eval' (Right val) = eval (List (val : xs))

  eval (List ((Func e args body) : xs)) = bindArguments
    >>= evaluateInContext body
    where
      bindArguments
        | length args == length xs = (++ e) . zip args <$> traverse eval xs
        | otherwise                = fail invalidNumberOfArgs
      invalidNumberOfArgs =
        "Invalid number of arguments provided. Expected "
          ++ show (length args)
          ++ ", received "
          ++ show (length xs)

  eval (List [x     ]) = eval x
  eval (List (x : xs)) = eval x >> eval (List xs)


-- IO

instance (Monad m) => MonadFail (ResultT m) where
  fail = liftResultT . Left

instance Eval (ResultT IO) where
  getEnv      = ResultT get
  setEnv      = ResultT . put
  primitives' = basicPrimitives

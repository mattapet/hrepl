{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Lisp.Eval
  ( Eval(getEnv, readC, readLine, setEnv, write)
  , eval
  , ResultT(..)
  , liftResultT
  , liftResultM
  , runResultT
  ) where

import           Control.Applicative
import           Data.ExceptT
import           Data.StateT

import           Lisp.Core
import           Lisp.Primitives                ( Primitive
                                                , primitives
                                                )
import           System.IO                      ( hFlush
                                                , stdout
                                                )

newtype ResultT m a = ResultT (StateT Environment (ExceptT String m) a)
  deriving (Monad, Applicative, Functor)

liftResultT :: (Applicative m) => Either String a -> ResultT m a
liftResultT = ResultT . liftStateT . liftExceptT

liftResultM :: (Monad m) => m a -> ResultT m a
liftResultM = ResultT . liftStateT . ExceptT . (Right <$>)

runResultT :: Environment -> ResultT m a -> m (Either String (a, Environment))
runResultT env (ResultT r) = runExceptT $ runStateT r env

class (Monad m, MonadFail m) => Eval m where
  getEnv :: m Environment
  setEnv :: Environment -> m ()
  write :: String -> m ()
  readC :: m Char
  readLine :: m String

-- | Variable lookup retrieves a variable from the entire accessible context.
--   I.e., current environment as `Right` value, or one of the primitive
--   functions as `Left` value.
lookupVariable :: (Eval m) => Name -> m (Either (Primitive m) Expr)
lookupVariable n = do
  env <- getEnv
  convertToState ((Right <$> lookup n env) <|> (Left <$> lookup n primitives))
  where
    convertToState = maybe unboundError return
    unboundError   = fail $ unboundVariableFound n

evaluateInContext :: (Eval m) => Expr -> Environment -> m Expr
evaluateInContext content context = do
  currentEnv <- getEnv
  result     <- setEnv context >> eval content
  setEnv currentEnv >> return result

-- Evaluation

eval :: (Eval m) => Expr -> m Expr

-- Atoms

eval val@(List      []) = return val
eval val@(Boolean   _ ) = return val
eval val@(Number    _ ) = return val
eval val@(StringLit _ ) = return val
eval val@Func{}         = return val
eval val@Quote{}        = return val
eval (Identifier x)     = lookupVariable x >>= unpack
  where
    -- There is nothing more we can do with primitive value, so we just return
    -- their identifier back
    unpack (Left  _) = return $ Identifier x
    unpack (Right e) = eval e

-- IO

eval (List [Identifier "write", expr]) = do
  value <- eval expr
  _     <- write (format value)
  return $ List []

eval (List [Identifier "write-line", expr]) = do
  value <- eval expr
  _     <- write $ format value ++ "\n"
  return $ List []

eval (List [Identifier "read"]) = StringLit . (: []) <$> readC
eval (List [Identifier "read-line"]) = StringLit <$> readLine

-- If

eval (List [Identifier "if", cond', then', else']) = eval cond' >>= \case
  Boolean True  -> eval then'
  Boolean False -> eval else'
  _             -> fail ifInvalidArgumentType
eval (List ((Identifier "if") : _)) = fail ifInvalidNumberOfArguments

-- Let

eval (List [Identifier "let", List bindings, body]) = do
  env             <- getEnv
  (names, values) <- unzip <$> unpackBindings bindings
  eval $ List $ Func env names body : values
  where
    unpackBindings = traverse unpackBinding
    unpackBinding (Identifier x) = return (x, List [])
    unpackBinding (List [Identifier x, value]) = return (x, value)
    unpackBinding _ = fail invalidLetBindingSymbol

-- Lambda definition declaration

eval (List (Identifier "lambda" : List args : body)) = do
  env   <- getEnv
  args' <- unpackArgs args
  return $ Func env args' (List body)
  where
    unpackArgs = traverse unpackArg
    unpackArg (Identifier n) = return n
    unpackArg _              = fail invalidLambdaMissingIdentifier

eval (List ((Identifier "lambda")         : _   )) = fail invalidLambda

-- Function declaration

eval (List (Identifier "defun" : Identifier name : List args : body)) = do
  env   <- getEnv
  args' <- unpackArgs args
  let func = Func env' args' (List body)
      env' = (name, func) : env
  setEnv env' >> return (List [])
  where
    unpackArgs = traverse unpackArg
    unpackArg (Identifier n) = return n
    unpackArg _              = fail invalidFuncDefMissingIdentifier

eval (List ((Identifier "defun") : _ )) = fail invalidFuncDef


-- Applications
eval (List ((Identifier op     ) : xs)) = lookupVariable op >>= eval'
  where
    eval' (Left prim) = traverse eval xs >>= prim >>= \case
      Right x -> return x
      Left  e -> fail e
    eval' (Right val) = eval (List (val : xs))

eval (List ((Func e args body) : xs)) = do
  bindArguments >>= evaluateInContext body
  where
    bindArguments
      | length args == length xs = (++ e) . zip args <$> traverse eval xs
      | otherwise                = fail $ invalidNumberOfArguments args xs

eval (List [x     ]) = eval x
eval (List (x : xs)) = do
  x' <- eval x
  if x == x' then eval (List xs) else eval (List (x' : xs))


-- Error messages

unboundVariableFound :: Name -> String
unboundVariableFound n = "Found unbound variable '" ++ n ++ "'"

ifInvalidArgumentType :: String
ifInvalidArgumentType =
  "Invalid argument type. Condition must evaluate to Boolean"

ifInvalidNumberOfArguments :: String
ifInvalidNumberOfArguments =
  "Invalid number of arguments. 'if' expects exactly three arguments"

invalidFuncDefMissingIdentifier :: String
invalidFuncDefMissingIdentifier =
  "Invalid function definition. Argument name must be an identifier"

invalidFuncDef :: String
invalidFuncDef =
  "Invalid function definition. Function expects function name, list of arguments and body"

invalidLambdaMissingIdentifier :: String
invalidLambdaMissingIdentifier =
  "Invalid lambda definition. Argument name must be an identifier"

invalidLambda :: String
invalidLambda =
  "Invalid lambda definition. Function expects function name, list of arguments and body"


invalidNumberOfArguments :: [a] -> [b] -> String
invalidNumberOfArguments expected received =
  "Invalid number of arguments provided. Expected "
    ++ show (length expected)
    ++ ", received "
    ++ show (length received)

invalidLetBindingSymbol :: String
invalidLetBindingSymbol = "Invalid 'let' binding symbol."

-- IO

instance (Monad m) => MonadFail (ResultT m) where
  fail = liftResultT . Left

instance Eval (ResultT IO) where
  getEnv = ResultT get
  setEnv = ResultT . put
  write c = liftResultM (putStr c >> hFlush stdout)
  readC    = liftResultM getChar
  readLine = liftResultM getLine

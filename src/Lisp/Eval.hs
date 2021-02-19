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
  maybeToState ((Right <$> lookupEnv n env) <|> (Left <$> lookupPrimitive n))
  where
    maybeToState = maybe unboundError return
    unboundError = failWith $ "Found unbound variable '" ++ n ++ "'"

evaluateInContext :: Environment -> Expr -> Result Expr
evaluateInContext context content = do
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

-- Applications
eval (Application x xs) = eval x >>= apply
  where
    apply (Identifier op) = lookupVariable op >>= \case
      Left  prim -> mapM eval xs >>= liftF . prim
      Right e    -> eval (Application e xs)
    apply (Func e' args body) =
      bindArguments e' args >>= flip evaluateInContext body
    apply _ = failWith "Type is not callable"
    bindArguments e args | length args == length xs = return $ e ++ zip args xs
                         | otherwise = failWith $ invalidNumberOfArgs args
    invalidNumberOfArgs args =
      "Invalid number of arguments provided. Expected "
        ++ show (length args)
        ++ ", received "
        ++ show (length xs)

-- Function declaration
eval (FuncDef name args body) = do
  func <- captureFunctionInContext
  bindToVariable func >> return func
  where
    captureFunctionInContext = get >>= \env -> return $ Func env args body
    bindToVariable f = get >>= put . (++ [(name, f)])

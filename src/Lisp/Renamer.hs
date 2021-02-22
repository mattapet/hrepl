{-# LANGUAGE TupleSections #-}

module Lisp.Renamer
  ( rename
  ) where

import           Control.Monad                  ( (>=>)
                                                , void
                                                )
import           Data.Map                hiding ( lookup )
import           Data.StateT
import           Lisp.Core

type Result a = StateT (Map Name Int) (Either String) a

rename :: Expr -> Either String Expr
rename expr = fst <$> runStateT (rename' expr) empty

rename' :: Expr -> Result Expr
rename' val@Func{}         = return val
rename' val@(Boolean    _) = return val
rename' val@(Number     _) = return val
rename' val@(StringLit  _) = return val
rename' (    Quote      e) = Quote <$> rename' e

rename' (    Identifier x) = Identifier <$> renameIdentifier x

rename' (List [Identifier "defun", Identifier n, List args, body]) = do
  (args', body') <- renameContext args body
  return (List [Identifier "defun", Identifier n, List args', body'])

rename' (List [Identifier "lambda", List args, body]) = do
  (args', body') <- renameContext args body
  return (List [Identifier "lambda", List args', body'])

rename' (List [Identifier "let", List bindings, body]) = do
  (args , bindings') <- unzip <$> unpackBindings bindings
  (args', body'    ) <- renameContext args body
  return (List [Identifier "let", List $ bind $ zip args' bindings', body'])
  where
    unpackBindings = traverse unpackBinding
    unpackBinding (List (x : xs)) =
      (\xs' -> (x, List . (: xs'))) <$> traverse rename' xs
    unpackBinding x = return (x, id)
    bind []                    = []
    bind ((arg, binding) : xs) = binding arg : bind xs

rename' (List xs) = List <$> traverse rename' xs

renameContext :: [Expr] -> Expr -> Result ([Expr], Expr)
renameContext args body = do
  env   <- get
  _     <- scopeArguments args
  args' <- traverse rename' args
  body' <- rename' body
  _     <- put env
  return (args', body')

scopeArguments :: [Expr] -> Result ()
scopeArguments = void . traverse (unpack >=> scopeArgument)
  where
    unpack (Identifier x) = return x
    unpack _              = error "Expected identifier"

scopeArgument :: Name -> Result ()
scopeArgument x = do
  env <- get
  let nextId = maybe 1 (+ 1) $ env !? x
  put (insert x nextId env)

renameIdentifier :: Name -> Result Name
renameIdentifier x = do
  env <- get
  case env !? x of
    Just id' -> return $ x ++ "_$" ++ show id'
    Nothing  -> return x

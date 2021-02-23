{-# LANGUAGE TupleSections #-}

module Lisp.Renamer
  ( rename
  ) where

import           Control.Monad                  ( (>=>) )
import           Data.Map                hiding ( lookup )
import           Data.Maybe                     ( fromMaybe )
import           Data.StateT
import           Lisp.Core

type Result a = StateT (Map Name Int) (Either String) a

rename :: Expr -> Either String Expr
rename expr = fst <$> runStateT (rename' [] expr) empty

rename' :: [(Name, Name)] -> Expr -> Result Expr
rename' _  val@Func{}         = return val
rename' _  val@(Boolean    _) = return val
rename' _  val@(Number     _) = return val
rename' _  val@(StringLit  _) = return val
rename' ns (    Quote      e) = Quote <$> rename' ns e

rename' ns (    Identifier x) = Identifier <$> renameIdentifier ns x

rename' ns (List (Identifier "defun" : Identifier n : List args : body)) = do
  (args', body') <- renameContext ns args body
  return (List (Identifier "defun" : Identifier n : List args' : body'))

rename' ns (List (Identifier "lambda" : List args : body)) = do
  (args', body') <- renameContext ns args body
  return (List (Identifier "lambda" : List args' : body'))

rename' ns (List (Identifier "let" : List bindings : body)) = do
  (args , bindings') <- unzip <$> unpackBindings bindings
  (args', body'    ) <- renameContext ns args body
  return (List (Identifier "let" : List (bind $ zip args' bindings') : body'))
  where
    unpackBindings = traverse unpackBinding
    unpackBinding (List (x : xs)) =
      (\xs' -> (x, List . (: xs'))) <$> traverse (rename' ns) xs
    unpackBinding x = return (x, id)
    bind []                    = []
    bind ((arg, binding) : xs) = binding arg : bind xs

rename' ns (List xs) = List <$> traverse (rename' ns) xs

renameContext :: [(Name, Name)] -> [Expr] -> [Expr] -> Result ([Expr], [Expr])
renameContext ns args body = do
  ns'   <- renameArguments args
  args' <- traverse (rename' $ ns' ++ ns) args
  body' <- traverse (rename' $ ns' ++ ns) body
  return (args', body')

renameArguments :: [Expr] -> Result [(Name, Name)]
renameArguments = traverse (unpack >=> renameArgument)
  where
    unpack (Identifier x) = return x
    unpack _              = error "Expected identifier"

renameArgument :: Name -> Result (Name, Name)
renameArgument x = do
  env <- get
  let nextId = maybe 1 (+ 1) $ env !? x
  let x'     = x ++ "_$" ++ show nextId
  put (insert x nextId env)
  return (x, x')

renameIdentifier :: [(Name, Name)] -> Name -> Result Name
renameIdentifier ns x = return $ fromMaybe x $ lookup x ns

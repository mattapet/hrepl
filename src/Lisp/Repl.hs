{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Lisp.Repl where

import           Control.Monad                  ( (>=>) )
import           Lisp.Core
import           Lisp.Eval
import           Lisp.Parser
import           System.IO

import           Lisp.Renamer                   ( rename )

class (Monad m, Eval (ResultT m)) => Repl m where
  read' :: m String
  print' :: String -> m ()
  loadFile :: String -> m String

  eval' :: Environment -> String -> m Environment
  eval' env = runResultT env . runEval >=> \case
      Right (x, env') -> print' x >> return env'
      Left err -> print' err >> return env
    where
      runEval = liftResultT . parseExpr >=> liftResultT . rename >=> eval >=> return . format

loop :: (Repl m) => Environment -> m ()
loop env = do
  input <- read'
  case words input of
    (":quit"              : _) -> return ()
    (":env"               : _) -> print' (formatEnv env) >> loop env
    (":load" : fileName   : _) -> loadFile fileName >>= eval' env >>= loop
    (":t"    : symbolName : _) -> case lookup symbolName env of
      Just x  -> print' (show x) >> loop env
      Nothing -> loop env
    _ -> eval' env input >>= loop

-- Repl IO implementation

instance Repl IO where
  read'    = putStr "hrepl> " >> hFlush stdout >> getLine
  print'   = putStrLn
  loadFile = readFile

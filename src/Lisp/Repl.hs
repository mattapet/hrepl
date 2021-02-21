{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Lisp.Repl where

import           Control.Monad                  ( (>=>) )
import           Lisp.Core
import           Lisp.Eval
import           Lisp.Parser
import           System.IO

class (Monad m) => Repl m where
  read' :: m String
  print' :: String -> m ()
  loadFile :: String -> m String

  eval' :: Environment -> String -> m Environment
  eval' env = runResultM env . runEval >=> \case
      Right (result, env') -> print' result >> return env'
      Left  err            -> print' err >> return env
    where
      runEval = liftResultM . parseExpr >=> eval >=> return . format

loop :: (Repl m) => Environment -> m ()
loop env = do
  input <- read'
  case words input of
    (":quit"            : _) -> return ()
    (":env"             : _) -> print' (formatEnv env) >> loop env
    (":load" : fileName : _) -> loadFile fileName >>= eval' env >>= loop
    _                        -> eval' env input >>= loop

-- Repl IO implementation

newtype ReplIO a = ReplIO { unReplIO :: IO a }
  deriving (Monad, Applicative, Functor)

instance Repl ReplIO where
  read'    = ReplIO $ putStr "hrepl> " >> hFlush stdout >> getLine
  print'   = ReplIO . putStrLn
  loadFile = ReplIO . readFile

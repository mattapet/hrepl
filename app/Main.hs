{-# LANGUAGE EmptyCase #-}

module Main where

import           Control.Monad                  ( (>=>)
                                                , unless
                                                )
import           Data.StateT
import           Lisp.Core
import           Lisp.Eval
import           Lisp.Parser
import           System.IO

main :: IO ()
main = loop []
  where
    loop :: Environment -> IO ()
    loop env = do
      input <- read'
      case input of
        ":quit" -> return ()
        ":env"  -> print' (show env) >> loop env
        _       -> case runStateT (eval' input) env of
          Right (result, env') -> print' result >> loop env'
          Left  err            -> print' err >> loop env

read' :: IO String
read' = putStr "hrepl> " >> hFlush stdout >> getLine

eval' :: String -> StateT Environment (Either String) String
eval' = liftF . parseExpr >=> eval >=> (return . format)

print' :: String -> IO ()
print' = putStrLn

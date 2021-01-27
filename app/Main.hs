module Main where

import           Control.Monad (unless)
import           StringCalc    (eval)
import           System.IO

main :: IO ()
main = do
  input <- read'
  unless (input == ":quit")
       $ print' (eval' input)
      >> main

eval' :: String -> String
eval' = eval

read' :: IO String
read' = putStr "hrepl> "
    >> hFlush stdout
    >> getLine

print' :: String -> IO ()
print' = putStrLn

module Main where

import           Control.Monad                  ( unless )
import           Data.Map.Strict               as Map
import           StringCalc                     ( eval )
import           System.IO

main :: IO ()
main = loop Map.empty
  where
    loop state = do
      input <- read'
      unless (input == ":quit")
        $ let (r, state') = eval state input in print' r >> loop state'

read' :: IO String
read' = putStr "hrepl> " >> hFlush stdout >> getLine

print' :: String -> IO ()
print' = putStrLn

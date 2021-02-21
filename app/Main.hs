{-# LANGUAGE EmptyCase #-}

module Main where

import           Lisp.Repl

main :: IO ()
main = unReplIO $ loop []

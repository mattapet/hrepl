{-# LANGUAGE FlexibleInstances #-}

module Lisp.Lib where

import Data.Functor.Identity
import Data.StateT
import Lisp.Eval
import Lisp.Primitives
import Lisp.Core

type Result a = ResultT Identity a

runResult :: Environment -> Result a -> Either String (a, Environment)
runResult env = runIdentity . runResultT env

liftResult :: Either String a -> Result a
liftResult = liftResultT

-- Result instances

instance Eval (ResultT Identity) where
  getEnv      = ResultT get
  setEnv      = ResultT . put
  primitives' = basicPrimitives

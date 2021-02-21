{-# LANGUAGE FlexibleInstances #-}

module Lisp.Lib where

import           Data.Functor.Identity
import qualified Data.State                    as S
import qualified Data.StateT                   as ST
import           Lisp.Core
import           Lisp.Eval
import           Lisp.Primitives

-- Identity Result specialization

type Result a = ResultT Identity a

runResult :: Environment -> Result a -> Either String (a, Environment)
runResult env = runIdentity . runResultT env

liftResult :: Either String a -> Result a
liftResult = liftResultT

instance Eval (ResultT Identity) where
  getEnv = ResultT ST.get
  setEnv = ResultT . ST.put
  write _ = liftResultM $ Identity ()

-- State Result specialization

type ResultState s a = ResultT (S.State s) a

runResultState :: s
               -> Environment
               -> ResultState s a
               -> (Either String (a, Environment), s)
runResultState s env = flip S.runState s . runResultT env

instance Eval (ResultT (S.State (String, String))) where
  getEnv = ResultT ST.get
  setEnv = ResultT . ST.put
  write s = liftResultM $ S.State $ \(inp, out) -> ((), (inp, out ++ s))

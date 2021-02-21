{-# LANGUAGE TupleSections #-}

module Data.State where

import           Control.Applicative            ( liftA )
import           Control.Monad                  ( ap )

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap = liftA

instance Applicative (State s) where
  pure a = State (a, )
  (<*>) = ap

instance Monad (State s) where
  return = pure
  fa >>= f = State $ \s -> let (a, s') = runState fa s in runState (f a) s'

put :: s -> State s ()
put = State . const ((), )

get :: State s s
get = State $ \s -> (s, s)

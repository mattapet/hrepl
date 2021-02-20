{-# LANGUAGE TupleSections #-}

module Data.StateT where

import           Control.Monad                  ( ap )
import           Data.Bifunctor                 ( first )

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

-- Instances

instance (Functor m) => Functor (StateT s m) where
  fmap f ma = StateT $ fmap (first f) . runStateT ma

instance (Monad m) => Applicative (StateT s m) where
  pure  = return
  (<*>) = ap

instance (Monad m) => Monad (StateT s m) where
  return a = StateT $ \s -> pure (a, s)
  ma >>= f = StateT $ \s -> do
    (a, s' ) <- runStateT ma s
    (b, s'') <- runStateT (f a) s'
    return (b, s'')

-- Operations

get :: (Applicative m) => StateT s m s
get = StateT $ \s -> pure (s, s)

put :: (Applicative m) => s -> StateT s m ()
put s = StateT $ \_ -> pure ((), s)

liftS :: (Functor m) => m a -> StateT s m a
liftS ma = StateT $ \s -> (, s) <$> ma

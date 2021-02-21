{-# LANGUAGE LambdaCase #-}

module Data.ExceptT where

import           Control.Applicative

newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }

instance (Functor m) => Functor (ExceptT e m) where
  fmap f = ExceptT . fmap (fmap f) . runExceptT

instance (Applicative m) => Applicative (ExceptT e m) where
  pure = ExceptT . pure . Right
  fab <*> fa = ExceptT $ (<*>) <$> runExceptT fab <*> runExceptT fa

instance (Monad m, Monoid e) => Alternative (ExceptT e m) where
  empty = ExceptT $ return $ Left mempty
  lhs <|> rhs = ExceptT $ runExceptT lhs >>= \case
    Right a -> return $ Right a
    Left  e -> either (Left . (<> e)) Right <$> runExceptT rhs

instance (Monad m) => Monad (ExceptT e m) where
  return = pure
  ma >>= f = ExceptT $ runExceptT ma >>= \case
    Right a -> runExceptT (f a)
    Left  e -> return (Left e)

instance (MonadFail m) => MonadFail (ExceptT e m) where
  fail = ExceptT . fail

throwError :: (Monad m) => e -> ExceptT e m a
throwError = ExceptT . return . Left

catchError :: (Monad m)
           => (e -> ExceptT e' m a)
           -> ExceptT e m a
           -> ExceptT e' m a
catchError f m = ExceptT $ runExceptT m >>= \case
  Right a -> return (Right a)
  Left  e -> runExceptT (f e)

catch :: (Monad m) => ExceptT e m a -> (e -> ExceptT e' m a) -> ExceptT e' m a
catch = flip catchError

liftExceptT :: (Applicative m) => Either e a -> ExceptT e m a
liftExceptT = ExceptT . pure

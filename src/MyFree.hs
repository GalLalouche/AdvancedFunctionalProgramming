{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module MyFree where

import Control.Applicative (liftA2)
data Free f a = Pure a | Free (f (Free f a))

instance (Functor f) => Functor (Free f) where
  fmap f (Pure a) = Pure $ f a
  fmap f (Free fa) = Free $ fmap f <$> fa
instance (Functor f) => Applicative (Free f) where
  pure = Pure
  Pure f <*> fb = f <$> fb
  Free fa <*> fb = Free $ (<*> fb) <$> fa

instance (Functor f) => Monad (Free f) where
  Pure a >>= f = f a
  Free fa >>= f = Free $ (>>= f) <$> fa

foldFree :: Monad m => (forall x . f x -> m x) -> Free f a -> m a
foldFree _ (Pure a) = return a
foldFree f (Free fa) = f fa >>= foldFree f

newtype IOAction a = ReadFile a
data IOActions a = NoActions a | IOActions (IOAction (IOActions a)) deriving Functor
instance Applicative IOActions where
  pure = NoActions
  NoActions f <*> fb = f <$> fb
  IOActions fa <*> fb = IOActions $ (<*> fb) <$> fa
instance Monad IOActions where
  NoActions a >>= f = f a
  IOActions fa >>= f = IOActions $ (>>= f) <$> fa

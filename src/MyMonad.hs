module MyMonad where

import Prelude hiding (Monad(..), Applicative(..), Functor(..), Monoid(..), Semigroup(..), (=<<), (<$>))
import MyFunctor
import Data.Composition
import MyApplicative
import MySemigroup

class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>=) = join .: flip fmap
  (=<<) :: (a -> m b) -> m a -> m b
  (=<<) = flip (>>=)
  return :: a -> m a
  return = pure
  join :: m (m a) -> m a
  join = (=<<) id
  {-# MINIMAL join | (>>=) #-}

instance Monad ((->) r) where
  f >>= g = \x -> g (f x) x

instance Monoid w => Monad ((,) w) where
  (w1, a) >>= f = let (w2, b) = f a in (w1 <> w2, b)

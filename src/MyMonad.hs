{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module MyMonad where

import Prelude hiding (Functor(..), Applicative(..), Monad(..), (=<<))

import MyApplicative
import MyFunctor

class Applicative f => Monad f where
  return :: a -> f a
  return = pure
  (>>=) :: f a -> (a -> f b) -> f b

(=<<) :: Monad f => (a -> f b) -> f a -> f b
(=<<) = flip (>>=)

join :: Monad f => f (f a) -> f a
join = (=<<) id


--instance Monad f => Functor f where
--  fmap f = (=<<) (return . f)
--instance Monad f => Applicative f where
--  pure = return
--  liftA2 :: (a -> b -> c) -> f a -> f b -> f c
--  liftA2 f fa fb = fa >>= (\a -> fb >>= (\b -> return $ f a b))

instance Monad Maybe where
  Nothing >>= _ = Nothing
  (Just x) >>= f = f x

instance Monad [] where
  [] >>= _ = []
  (x : xs) >>= f = f x ++ (xs >>= f)

{-# LANGUAGE ScopedTypeVariables #-}
module MyFoldable where

import Prelude hiding (Foldable(..), Monoid(..), Semigroup(..))
import MySemigroup
import MyTree


x & f = f x
class Foldable t where
  foldr :: (a -> b -> b) -> b -> t a -> b
  -- we need a function from a to some Monoid m...
  -- All we have is a function from a -> b -> b...
  -- But as w saw in the previous lecture,
  -- (b -> b) is already a Monoid, we just have to wrap it with Endo
  foldr f b t = appEndo (foldMap (Endo . f) t) b
  foldMap :: Monoid m => (a -> m) -> t a -> m
  foldMap f = foldr (\a b -> f a <> b) mempty
  {-# MINIMAL foldr | foldMap #-}

instance Foldable Maybe where
  foldr _ b Nothing = b
  foldr f b (Just a) = f a b

instance Foldable [] where
  foldr _ b []       = b
  foldr f b (x : xs) = foldr f (f x b) xs

instance Foldable Tree where
  foldr _ b EmptyTree = b
  foldr f b (Tree a left right) = foldr f (f a (foldr f b left)) right

newtype Down a = Down a
-- Any wrapped value we saw is also foldable, for example
instance Foldable Down where
  foldr f b (Down a) = f a b

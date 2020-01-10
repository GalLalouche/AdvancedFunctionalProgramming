{-# LANGUAGE ScopedTypeVariables #-}
module MyFoldable where

import Prelude hiding (Foldable(..), Monoid(..), Semigroup(..), Ord(..), Ordering(..))
import MySemigroup
import MyTree
import MyOrd


x & f = f x
class Foldable t where
  foldr :: (a -> b -> b) -> b -> t a -> b
  foldr f b t = appEndo (foldMap (Endo . f) t) b
  foldMap :: Monoid m => (a -> m) -> t a -> m
  foldMap f = foldr (\a b -> f a <> b) mempty
  {-# MINIMAL foldr | foldMap #-}

  fold :: Monoid m => t m -> m
  fold = foldMap id
  toList :: t a -> [a]
  toList = foldMap (: [])

  null :: t a -> Bool
  null = getAny . foldMap (Any . const False)

  length :: t a -> Int
  length = getSum . foldMap (Sum . const 1)

  elem :: Eq a => a -> t a -> Bool
  elem e = getAny . foldMap (Any . (e == ))


any :: Foldable t => (a -> Bool) -> t a -> Bool
any p = getAny . foldMap (Any . p)
all :: Foldable t => (a -> Bool) -> t a -> Bool
all p = getAll . foldMap (All . p)
instance Foldable Maybe where
  foldr _ b Nothing = b
  foldr f b (Just a) = f a b

instance Foldable [] where
  foldr _ b []       = b
  foldr f b (x : xs) = f x (foldr f b xs)

instance Foldable Tree where
  foldr _ b EmptyTree = b
  foldr f b (Tree a left right) = let
      rAgg = foldr f b right
      currentAgg = f a rAgg
    in foldr f currentAgg left


-- Any wrapped value we saw is also foldable, for example
instance Foldable Down where
  foldMap f = f . getDown
  
-- Not associate Set
newtype Same a = Same { unSame :: a }
instance Eq (Same a) where (==) _ _ = True
instance Ord (Same a) where (compare) _ _ = EQ
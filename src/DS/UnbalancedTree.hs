{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module DS.UnbalancedTree where

import Prelude hiding (Ordering(..))
data Tree a = Empty | Tree (Tree a) a (Tree a) deriving Show
data Ordering = EQ | LT | GT
type Comparator a = a -> a -> Ordering

member :: forall a. Comparator a -> a -> Tree a -> Bool
member compare e = go where
  go :: Tree a -> Bool
  go Empty = False
  go (Tree l x r) = case compare e x of
    EQ -> True
    LT -> go l
    GT -> go r

memberVT :: forall a. Comparator a -> a -> Tree a -> Bool
memberVT compare e = go where
  go :: Tree a -> Bool
  go Empty = False
  go (Tree _ ((compare e) -> EQ) _) = True
  go (Tree l ((compare e) -> LT) _) = go l
  go (Tree _ ((compare e) -> GT) r) = go r

add :: forall a. Comparator a -> a -> Tree a -> Tree a
add compare e = go where
  go :: Tree a -> Tree a
  go Empty = Tree Empty e Empty
  go t@(Tree l x r) = case compare e x of
    EQ -> t
    LT -> Tree (go l) x r
    GT -> Tree l x (go r)

remove :: forall a. Comparator a -> a -> Tree a -> Tree a
remove compare e = go where
  go Empty = Empty
  go (Tree l x r) = case compare e x of
    EQ -> case max l of
      Nothing   -> r
      Just succ -> Tree (remove compare succ l) succ r
    LT -> Tree (go l) x r
    GT -> Tree l x (go r)
  max :: Tree a -> Maybe a
  max Empty = Nothing
  max (Tree _ e Empty) = Just e
  max (Tree _ _ r) = max r

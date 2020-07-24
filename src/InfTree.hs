module InfTree where

data Tree a = EmptyTree | Tree a (Tree a) (Tree a)

add a EmptyTree = Tree EmptyTree a EmptyTree
add a (Tree left b right) = Tree left b (add a right)

instance Foldable Tree where
  foldr f b (Tree a left right) = let
      rAgg = foldr f b right
      currentAgg = f a rAgg
    in foldr f currentAgg left

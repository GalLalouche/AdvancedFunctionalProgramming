module MyTree where

import Prelude hiding (elem)
data Tree a = EmptyTree | Tree a (Tree a) (Tree a) deriving Show

elem :: Ord a => a -> Tree a -> Bool
elem _ EmptyTree = False
elem e (Tree x left right) =
  e == x || elem e (if e < x then left else right)

add :: Ord a => a -> Tree a -> Tree a
add e EmptyTree = Tree e EmptyTree EmptyTree
add e tree@(Tree x left right) = case compare e x of
  EQ -> tree
  LT -> Tree x (add e left) right
  GT -> Tree x left (add e right)

remove :: Ord a => a -> Tree a -> Tree a
remove _ EmptyTree = EmptyTree
remove e (Tree x left right) = case compare e x of
  EQ -> case max left of
    Nothing -> right
    Just succ -> Tree succ (remove succ left) right
  LT -> Tree x (remove e left) right
  GT -> Tree x left (remove e right)
  where
    max EmptyTree = Nothing
    max (Tree e left right) = case right of
      EmptyTree -> Just e
      tree -> max tree

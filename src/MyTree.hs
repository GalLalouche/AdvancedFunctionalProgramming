module MyTree where

data Tree a = EmptyTree | Tree a (Tree a) (Tree a)

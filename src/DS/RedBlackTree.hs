module DS.RedBlackTree where

data Color = R | B deriving Show
data Tree a = Empty | Tree Color (Tree a) a (Tree a) deriving Show

member :: (Ord a) => a -> Tree a -> Bool
member _ Empty = False
member x (Tree _ l y r)
  | x == y    = True
  | x < y     = member x l
  | otherwise = member x r

insert :: (Ord a) => a -> Tree a -> Tree a
insert x = mkBlack . go where 
  go Empty  = Tree R Empty x Empty
  go t@(Tree c l y r)
    | x == y = t -- Nothing to be done
    | x < y  = balance c (go r) y l
    | x > y  = balance c l y (go r)
  mkBlack (Tree _ l y r) = Tree B l y r

balance :: Color -> Tree a -> a -> Tree a -> Tree a
balance = undefined
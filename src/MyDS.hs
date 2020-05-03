{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module MyDS where

import qualified Data.Tree as T (Tree(..))
import qualified Data.Tree.Pretty as Pretty
import Data.Tree (Tree)
tree = T.Node

data Node a = Node2 a a | Node3 a a a
data FingerTree a = Empty | Single a | Deep [a] (FingerTree (Node a)) [a]


prepend :: a -> FingerTree a -> FingerTree a
prepend a Empty = Single a
prepend a (Single b) = Deep [a] Empty [b]
prepend a (Deep [b,c,d,e] m sf) = Deep [a, b] (prepend (Node3 c d e) m) sf
prepend a (Deep pr m sf) = Deep (a : pr) m sf

append :: a -> FingerTree a -> FingerTree a
append a Empty = Single a
append a (Single b) = Deep [b] Empty [a]
append a (Deep pr m [e, d, c, b]) = Deep pr (append (Node3 e d c) m) [b, a]
append a (Deep pr m sf) = Deep pr m (sf ++ [a])

instance Functor Node where
  fmap f (Node2 a b) = Node2 (f a) (f b)
  fmap f (Node3 a b c) = Node3 (f a) (f b) (f c)
instance Functor FingerTree where
  fmap _ Empty = Empty
  fmap f (Single a) = Single (f a)
  fmap f (Deep pr m sf) = Deep (fmap f pr) (fmap (fmap f) m) (fmap f sf)

class ToTreeString a where
  toTree :: a -> Tree String

draw :: ToTreeString a => a -> String
draw = Pretty.drawVerticalTree . toTree

conTree = tree "O"
instance ToTreeString Int where
  toTree x = tree (show x) []
instance ToTreeString Char where
  toTree x = tree (show x) []
instance ToTreeString String where
  toTree = (`tree` [])

instance ToTreeString a => ToTreeString (FingerTree a) where
  toTree Empty = tree "-" []
  toTree (Single a) = toTree a
  toTree (Deep pr m sf) = conTree [listToTree pr, toTree m, listToTree sf] where
    listToTree = conTree . fmap toTree

instance ToTreeString a => ToTreeString (Node a) where
  toTree (Node2 a b) = conTree [toTree a, toTree b]
  toTree (Node3 a b c) = conTree [toTree a, toTree b, toTree c]
fromList :: [a] -> FingerTree a
fromList = foldr prepend Empty
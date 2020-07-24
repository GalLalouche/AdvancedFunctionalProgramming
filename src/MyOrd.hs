{-# LANGUAGE NoMonomorphismRestriction #-}
module MyOrd where

import Data.Composition ((.:))
import Prelude hiding (Ord(..), Down, Ordering(..), compare) -- avoid collisions
import qualified Prelude as P

data Ordering = LT | EQ | GT
instance Eq Ordering where
  LT == LT = True
  EQ == EQ = True
  GT == GT = True
  _  == _  = False
class Eq a => Ord a where
  compare :: a -> a -> Ordering
  compare x y
    | x == y    = EQ
    | x < y     = LT
    | otherwise = GT
  (<=) :: a -> a -> Bool
  (<=) = (/= GT) .: compare
  {-# MINIMAL (compare) | (<=) #-}

  (>=) :: a -> a -> Bool
  (>=) = flip (<=)
  (>) :: a -> a -> Bool
  (>) = not .: (<=)
  (<) :: a -> a -> Bool
  (<) = flip (>)
  max :: a -> a -> a
  max x y = if x <= y then y else x
  min :: a -> a -> a
  min = flip max


newtype Down a = Down { getDown :: a }
instance Eq a => Eq (Down a) where
  (Down x) == (Down y) = x == y
instance Ord a => Ord (Down a) where
  compare (Down x) (Down y) = compare x y
  
class IsPalindrome a where
  isPalindrome :: a -> Bool
instance IsPalindrome Int where
  isPalindrome = undefined
instance IsPalindrome [a] where
  isPalindrome = undefined

instance Ord a => Ord (Maybe a) where
  Nothing  <= _        = True
  _        <= Nothing  = False
  Just x <= Just y     = x <= y

instance Ord a => Ord [a] where
  []       <= _        = True
  _        <= []       = False
  (x : xs) <= (y : ys) = x <= y && xs <= ys

bubbleSort :: Ord a => [a] -> [a]
bubbleSort xs =
  if isSorted xs then xs else bubbleSort $ go xs where
    isSorted []           = True
    isSorted [x]          = True
    isSorted (x : y : xs) = (x < y) && isSorted (y : xs)
    go []           = []
    go [x]          = [x]
    go (x : y : xs) =
      if x > y then y : go (x : xs) else x : go (y : xs)
--
--max :: Ord a => [a] -> Maybe a
--max [] = Nothing
--max (x : xs) = Just $ case max xs of
--  Nothing -> x
--  Just y -> if x < y then y else x 


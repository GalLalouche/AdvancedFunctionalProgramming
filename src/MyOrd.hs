module MyOrd where

import Prelude hiding (Ord, (<), max, Down, Ordering, compare) -- avoid collisions
import qualified Prelude as P

data Ordering = LT | EQ | GT
class Ord a where
  compare :: a -> a -> Ordering
  
newtype Down a = Down a
instance Ord a => Ord (Down a) where
  compare (Down x) (Down y) = compare x y
  
class IsPalindrome a where
  isPalindrome :: a -> Bool
instance IsPalindrome Int where
  isPalindrome = undefined
instance IsPalindrome [a] where
  isPalindrome = undefined

--
--max :: Ord a => [a] -> Maybe a
--max [] = Nothing
--max (x : xs) = Just $ case max xs of
--  Nothing -> x
--  Just y -> if x < y then y else x 


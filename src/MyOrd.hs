module MyOrd where

import Prelude hiding (Ord, (<), max, Down, Ordering(..), compare) -- avoid collisions
import qualified Prelude as P

data Ordering = LT | EQ | GT deriving Eq
class Ord a where
  compare :: a -> a -> Ordering
  
newtype Down a = Down { getDown :: a }
instance Ord a => Ord (Down a) where
  compare (Down x) (Down y) = compare x y
  
class IsPalindrome a where
  isPalindrome :: a -> Bool
instance IsPalindrome Int where
  isPalindrome = undefined
instance IsPalindrome [a] where
  isPalindrome = undefined

min x y = let c = compare x y in if c == GT then x else y
max x y = let c = compare x y in if c == LT then y else x
--
--max :: Ord a => [a] -> Maybe a
--max [] = Nothing
--max (x : xs) = Just $ case max xs of
--  Nothing -> x
--  Just y -> if x < y then y else x 


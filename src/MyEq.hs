module MyEq where

import Prelude hiding (Eq, (==), (!=))
import Data.Char (ord)

class Eq a where
  (==) :: a -> a -> Bool
  
(!=) :: Eq a => a -> a -> Bool
x != y = not (x == y)


instance Eq Int where
  0 == 0 = True
  _ == 0 = False
  x == y = (x - y) == 0

instance Eq Char where
  x == y = ord x == ord y

instance Eq a => Eq [a] where
  [] == [] = True
  _ == [] = False
  [] == _ = False
  (x : xs) == (y : ys) = x == y && xs == ys
  
instance Eq a => Eq (Maybe a) where
  Nothing == Nothing = True
  (Just x) == (Just y) = x == y
  _ == _ = False
  

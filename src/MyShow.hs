module MyShow where

import Prelude hiding (Show, show)

class Show a where
  show :: a -> String
  
instance Show Int where
  show 0 = "0"
  show 1 = "1"
  -- etc.
  show 9 = "9"
  show x = if x < 0 then "-" ++ show x else show (x `div` 10) ++ show (x `mod` 10)
  
instance Show Char where
  show x = [x]
  
instance Show a => Show (Maybe a) where
  show Nothing = "Nothing"
  show (Just x) = "Just " ++ show x
instance Show a => Show [a] where
  show [] = ""
  show (x : xs) = show x ++ show xs

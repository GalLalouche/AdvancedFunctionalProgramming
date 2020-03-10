module MyLists where

import           Data.Composition ((.:))
import           Prelude          (Bool (..), Eq (..), Int (..), Num (..), (.))

head :: [a] -> a
head (x : _) = x
tail :: [a] -> [a]
tail (_ : xs) = xs

length :: [a] -> Int
length [] = 0
length xs = 1 + length (tail xs)

null :: [a] -> Bool -- or isEmpty in other languages
null [] = True

take :: Int -> [a] -> [a]
take 0 _        = []
take _ []       = []
take n (x : xs) = x : take (n - 1) xs

drop :: Int -> [a] -> [a]
drop 0 xs       = xs
drop _ []       = []
drop n (_ : xs) = drop (n - 1) xs
filter :: (a -> Bool) -> [a] -> [a]
filter _ []       = []
filter p (x : xs) = 
  let rest = filter p xs 
  in if p x then x : rest else rest

map :: (a -> b) -> [a] -> [b]
map _ []       = []
map f (x : xs) = f x : map f xs

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x : xs) ++ ys = x : (xs ++ ys)

concat :: [[a]] -> [a] -- or flatten
concat []       = []
concat (x : xs) = x ++ concat xs
concatMap :: (a -> [b]) -> [a] -> [b] -- or flatMap
concatMap = concat .: map

reverseSquared :: [a] -> [a]
reverseSquared []       = []
reverseSquared (x : xs) = reverseSquared xs ++ [x]

reverseLinear :: [a] -> [a] -- manual tail recursion
reverseLinear xs = go xs [] where
  go [] xs       = xs
  go (x : xs) ys = go xs (x : ys)

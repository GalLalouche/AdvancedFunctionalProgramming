module MyListsWithFolds where

import           Data.Composition ((.:))
import           Prelude          (Bool (..), Eq (..), Int (..), Num (..), (.), flip, (||))

head :: [a] -> a
head (x : _) = x
tail :: [a] -> [a]
tail (_ : xs) = xs

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ b [] = b
foldl f b (x : xs) = foldl f (f b x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ b [] = b
foldr f b (x : xs) = f x (foldr f b xs)

null :: [a] -> Bool
null = foldr (\_ _ -> False) True

sum = foldr (+) 0
product = foldr (*) 1
length = foldr (\_ s -> s + 1) 0
filter p = foldr (\ x xs -> if p x then x : xs else xs) []
map f = foldr (\ x xs -> f x : xs) []
reverse = foldl (flip (:)) []
elem2 :: Int -> [Int] -> Bool
elem2 x = foldr (\ y b -> y == x || b) False

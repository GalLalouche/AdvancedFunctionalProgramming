module MyListsWithFolds where

import           Data.Composition ((.:))
import           Prelude          (Bool (..), Eq (..), Int (..), Num (..), (.), flip, Maybe(..), const, (++))

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
concat = foldr (++) []
headOpt :: [a] -> Maybe a
headOpt = foldr (const . Just) Nothing

--take :: Int -> [a] -> [a]
--take 0 _        = []
--take _ []       = []
--take n (x : xs) = x : take (n - 1) xs
--
--drop :: Int -> [a] -> [a]
--drop 0 xs       = xs
--drop _ []       = []
--drop n (_ : xs) = drop (n - 1) xs
--filter :: (a -> Bool) -> [a] -> [a]
--filter _ []       = []
--filter p (x : xs) = if p x then x : filter p xs else filter p xs
--
--map :: (a -> b) -> [a] -> [b]
--map _ []       = []
--map f (x : xs) = f x : map f xs
--
--(++) :: [a] -> [a] -> [a]
--[] ++ ys = ys
--(x : xs) ++ ys = x : (xs ++ ys)
--
--concat :: [[a]] -> [a] -- or flatten
--concat []       = []
--concat (x : xs) = x ++ concat xs
--concatMap :: (a -> [b]) -> [a] -> [b] -- or flatMap
--concatMap = concat .: map

reverseSquared :: [a] -> [a]
reverseSquared = foldl (flip (:)) []

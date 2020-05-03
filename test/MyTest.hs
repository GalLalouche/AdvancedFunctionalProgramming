module MyTest where

import Test.QuickCheck

testSort :: Ord a => ([a] -> [a]) -> [a] -> Bool
testSort sort xs =
  let sorted = sort xs
  in isPermutation sorted && isMonotonicallyIncreasing sorted where
    -- O(n^2), since we can't use sort...
    isPermutation = all (`elem` xs)
    isMonotonicallyIncreasing [] = True
    isMonotonicallyIncreasing [_] = True
    isMonotonicallyIncreasing (x : y : ys) = x <= y && isMonotonicallyIncreasing (y : ys)

lazyBubbleSort :: Ord a => [a] -> [a]
lazyBubbleSort [] = []
lazyBubbleSort [x] = [x]
lazyBubbleSort (x : y : ys) = if x > y
                              then y : lazyBubbleSort (x : ys)
                              else x : lazyBubbleSort (y : ys)
prop_check xs = testSort $ lazyBubbleSort xs
--
--max :: Ord a => [a] -> Maybe a
--max [] = Nothing
--max (x : xs) = Just $ case max xs of
--  Nothing -> x
--  Just y -> if x < y then y else x 


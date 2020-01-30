{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}


module MyQuickCheck where
import           Control.Applicative
import           Data.Composition

import           Data.Maybe          (fromMaybe)
import           MyRng

first :: [Maybe a] -> Maybe a
first []              = Nothing
first ((Just x) : xs) = Just x
first (_ : xs)        = first xs

random = Random gccConfig 0
property1 :: forall a. Randomable a => (a -> Bool) -> Maybe a
property1 p = aux 100 random where
  aux :: Int -> Random -> Maybe a
  aux 0 _ = Nothing
  aux n r = let
      (v, newR) = runRandom gen r
    in if p v then aux (n - 1) newR else Just $ fromMaybe v (auxShrink v)
  auxShrink v = if p v then Nothing else Just $ fromMaybe v (first $ map auxShrink (shrink v))

instance (Randomable a, Randomable b) => Randomable (a, b) where
  gen = liftA2 (,) gen gen
  -- Interesting show case for class: you can forget about mapping original a and b when shrinking, which can cause bugs
--  shrink (a, b) = liftA2 (,) (shrink a) (shrink b)
  shrink (a, b) = tail $ liftA2 (,) (a : shrink a) (b : shrink b)

property2 :: forall a b. (Randomable a, Randomable b) => (a -> b -> Bool) -> Maybe (a, b)
property2 p = aux 100 random where
  aux :: Int -> Random -> Maybe (a, b)
  aux 0 _ = Nothing
  aux n r = let
      (t@(v1, v2), newR) = runRandom gen r
    in if p v1 v2 then aux (n - 1) newR else Just $ fromMaybe t (auxShrink t)
  auxShrink t@(v1, v2) = if p v1 v2 then Nothing else Just $ fromMaybe t (first $ map auxShrink (shrink t))

newtype SmallInt = SmallInt Int deriving
  -- Magic for making SmallInt behave like a proper Int
  (Show, Eq, Ord, Num, Real, Enum, Integral)
instance Randomable SmallInt where
  gen = SmallInt <$> genMax 1000
  shrink i = filter (\i -> i > 0) $ [i `div` 10, i `div` 2, i - 1]
squareIsPositive :: SmallInt -> Bool
squareIsPositive x = x * x >= 0

piIsGood :: Double -> SmallInt -> Bool
piIsGood pi i = let res = sin (fromIntegral(i * 2) * pi) - 0 in abs (res - 0) < 0.1
property_squareIsPositive = property1 squareIsPositive
property_approx1 = property1 $ piIsGood (22.0 / 7)
property_approx2 = property1 $ piIsGood pi

noRem :: (Integral a, Num a, Eq a) => a -> a -> Bool
noRem 0 _ = True
noRem _ 0 = True
noRem x y = x `rem` y == 0
hasRem x y = not $ noRem x y

goodGcd x 0 = x
goodGcd x y = goodGcd y (x `mod` y)
badGcd x y = if noRem x y then y else if noRem y x then x else 1

isGoodGcd gcd x y = let
    res = gcd x y
  in noRem x res && noRem y res && all (\g -> hasRem x g || hasRem y g || noRem res g) [1..(min x y) - 1]

property_checkGoodGcd = property2 $ isGoodGcd (goodGcd :: SmallInt -> SmallInt -> SmallInt)
-- This shrink to (4, 10), which isn't minimal. That's because 10 can't be shrunk to 6 given the basic shrinkings.
property_checkBadGcd = property2 $ isGoodGcd (badGcd :: SmallInt -> SmallInt -> SmallInt)

reverseGood [] = []
reverseGood (x : xs) = (reverseGood xs) ++ [x]
reverseBad [] = []
reverseBad (x : xs) = x : (reverseBad xs)

isGoodReverse :: ([SmallInt] -> [SmallInt]) -> [SmallInt] -> Bool
isGoodReverse rev list = let r = rev list in rev r == list && (length list <= 1 || r /= list)
property_checkReverseGood = property1 $ isGoodReverse reverseGood
property_checkReverseBad = property1 $ isGoodReverse reverseBad

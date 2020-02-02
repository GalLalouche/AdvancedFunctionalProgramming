{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{- HLINT ignore "Use camelCase" -}


module MyQuickCheck where
import           Control.Applicative
import           Data.Composition
import           Data.Monoid         (First (..))

import           Data.Foldable       (find)
import           Data.Maybe          (fromMaybe)
import           MyRng


random = Random gccConfig 0
property1 :: forall a. (Randomable a, Show a) => (a -> Bool) -> Maybe a
property1 p = aux 100 random where
  aux :: Int -> Random -> Maybe a
  aux 0 _ = Nothing
  aux n r = let
      (v, newR) = runRandom gen r
    in if p v then aux (n - 1) newR else Just $ recShrink v
  recShrink v = case find (not . p) (shrink v) of
    Nothing -> v
    Just v2 -> recShrink v2


instance (Randomable a, Randomable b) => Randomable (a, b) where
  gen = liftA2 (,) gen gen
  -- Interesting show case for class: you can forget about mapping original a and b when shrinking, which can cause bugs
--  shrink (a, b) = liftA2 (,) (shrink a) (shrink b)
  shrink (a, b) = tail $ liftA2 (,) (a : shrink a) (b : shrink b)

property2 :: forall a b. (Show a, Randomable a, Show b, Randomable b) => (a -> b -> Bool) -> Maybe (a, b)
property2 = property1 . uncurry

newtype SmallInt = SmallInt Int deriving
  -- Magic for making SmallInt behave like a proper Int
  (Show, Eq, Ord, Num, Real, Enum, Integral)
instance Randomable SmallInt where
  gen = SmallInt <$> genMax 1000
  shrink i = filter (> 0) [i `div` 10, i `div` 2, i - 1]
squareIsPositive :: SmallInt -> Bool
squareIsPositive x = x * x >= 0

piIsGood :: Double -> Int -> Bool
piIsGood pi k = abs (sin $ fromIntegral k * pi) < 0.1

piIsGood2 :: Double -> Int -> Bool
piIsGood2 pi k = abs (sin (pi / 2 + fromIntegral k * 2 * pi) - 1) < 0.1
property_squareIsPositive = property1 squareIsPositive
property_approx1 = property1 $ piIsGood (22.0 / 7)
property_approx2 = property1 $ piIsGood pi
property_approx3 = property1 $ piIsGood pi
property_checkPi2Bad = property1 $ piIsGood2 (22.0 / 7)
property_checkPi2Good = property1 $ piIsGood2 pi
property_checkPi2Zero = property1 $ piIsGood2 0

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

newtype TinyInt = TinyInt Int
  -- Magic for making TinyInt behave like a proper Int
  deriving (Eq, Ord, Num, Real, Enum, Integral)
instance Show TinyInt where
  show (TinyInt x) = show x
instance Randomable TinyInt where
  gen = TinyInt <$> genMax 10
  shrink 0 = []
  shrink n = [n `div` 2, n - 1]

x `divides` y = x /= 0 && x `rem` y == 0
noRem = divides
hasRem x y = not $ x `divides` y

goodGcd x 0 = x
goodGcd x y = goodGcd y (x `mod` y)
badGcd x y | noRem x y = y | noRem y x = x | otherwise = 1

isGoodGcd gcd x y = let
    res = gcd x y
  in noRem x res && noRem y res && all (\g -> hasRem x g || hasRem y g || noRem res g) [1..min x y - 1]

property_checkGoodGcd = property2 $ isGoodGcd (goodGcd :: SmallInt -> SmallInt -> SmallInt)
-- This shrink to (4, 10), which isn't minimal. That's because 10 can't be shrunk to 6 given the basic shrinkings.
property_checkBadGcd = property2 $ isGoodGcd (badGcd :: SmallInt -> SmallInt -> SmallInt)

reverseGood []       = []
reverseGood (x : xs) = reverseGood xs ++ [x]
reverseBad = id
isGoodReverse :: ([SmallInt] -> [SmallInt]) -> [SmallInt] -> Bool
isGoodReverse rev list = let r = rev list in rev r == list && (length list <= 1 || r /= list)
property_checkReverseGood = property1 $ isGoodReverse reverseGood
property_checkReverseBad = property1 $ isGoodReverse reverseBad

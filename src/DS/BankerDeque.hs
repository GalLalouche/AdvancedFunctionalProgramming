{-# LANGUAGE ScopedTypeVariables #-}
module DS.BankerDeque where

import DS.Stack
import Prelude hiding (length, (++), reverse, empty, null)

data Deque a = Deque {l :: Stack a, r :: Stack a} deriving Show

-- Returns a pair of the first n elements of the given stack,
-- and the remaining elements, but the remaining elements are
-- reversed.
-- e.g., SplitReversed 3 [1, 2, 3, 4, 5, 6, 7] should return
-- [1, 2, 3] [7, 6, 5, 4]
splitReversed :: Int -> Stack a -> (Stack a, Stack a)
splitReversed = go empty where
  -- agg was aggregated reversed, to we re-reverse it.
  go agg 0 rest = (reverse agg, reverse rest)
  go agg n rest = case pop rest of
    Nothing -> (reverse agg, empty)
    Just (x, xs) -> go (push x agg) (n - 1) xs
c = 3

rebalance :: forall a . Deque a -> Deque a
rebalance d@(Deque l r)
  -- These are called guards and are an alternative to
  -- if then else if chains
  | ls > rs * c + 1 = leftToRight
  | rs > ls * c + 1 = rightToLeft
  | otherwise = d where
    ls = length l
    rs = length r
    totalSize = ls + rs
    halfSize = totalSize `div` 2
    leftToRight :: Deque a
    leftToRight = Deque l' r' where
      (l', toR) = splitReversed halfSize l
      r' = r ++ toR
    rightToLeft :: Deque a
    rightToLeft = Deque l' r' where
      (r', toL) = splitReversed halfSize r
      l' = l ++ toL

pushr :: a -> Deque a -> Deque a
pushr x (Deque l r) = rebalance $ Deque l (push x r)

pushl :: a -> Deque a -> Deque a
pushl x (Deque l r) = rebalance $ Deque (push x l) r

popl :: Deque a -> Maybe (a, Deque a)
popl (Deque l r)
  | null l && null r = Nothing
  | otherwise = case pop l of
    Nothing -> popl $ Deque (reverse r) empty
    Just (x, xs) -> Just (x, rebalance $ Deque xs r)
popr :: Deque a -> Maybe (a, Deque a)
popr (Deque l r)
  | null l && null r = Nothing
  | otherwise = case pop r of
    Nothing -> popr $ Deque empty (reverse l)
    Just (x, xs) -> Just (x, rebalance $ Deque l xs)


--instance Foldable Deque where
--  foldMap fm (Deque f _ b _) = foldMap fm f <> foldMap fm (reverse b)
--instance Traversable Deque where
--  traverse f d = frontOnly <$> traverse f (toList d)
--
--flatten :: Foldable f => f (Deque b) -> Deque b
--flatten = frontOnly . concatMap toList
--instance Functor Deque where
--  fmap fm (Deque f b) = Deque (fm <$> f) (fm <$> b)
--instance Applicative Deque where
--  pure x = Deque [x] []
--  liftA2 f d1 d2 = Deque (liftA2 f (toList d1) (toList d2)) []
--instance Monad Deque where

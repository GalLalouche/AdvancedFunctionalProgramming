module DeququeSmarter where

import           Control.Applicative (liftA2)
import           Data.Foldable       (toList)

data Dequeue a = Dequeue {front :: [a], frontSize :: Int, back :: [a], backSize :: Int} deriving Show

empty :: Dequeue a
empty = Dequeue [] 0 [] 0

splitReversed :: Int -> [a] -> ([a], [a])
splitReversed = go [] where
  go agg 0 rest = (reverse agg, reverse rest)
  go agg _ [] = (reverse agg, [])
  go agg n (x : xs) =  go (x : agg) (n - 1) xs
balanceRatio = 3
rebalance :: Dequeue a -> Dequeue a
rebalance dq@(Dequeue f fs b bs)
  | fs > bs * balanceRatio + 1 = frontToBack
  | bs > fs * balanceRatio + 1 = backToFront
  | otherwise = dq where
    joinedSize = fs + bs
    halfSize = joinedSize `div` 2
    frontToBack = Dequeue f' fs' (b ++ b') (bs + bs') where
      (f', b') = splitReversed halfSize f
      fs' = halfSize
      bs' = fs - halfSize
    backToFront = Dequeue (f ++ f') (fs + fs') b' bs' where
      (b', f') = splitReversed halfSize b
      bs' = halfSize
      fs' = bs - halfSize
isBalanced :: Dequeue a -> Bool
isBalanced (Dequeue _ fs _ bs) = fs <= bs * balanceRatio + 1 && bs <= fs * balanceRatio + 1
(<|) :: Dequeue a -> a -> Dequeue a
(Dequeue f fs b bs) <| x = rebalance $ Dequeue f fs (x : b) (bs + 1)
(|>) :: a -> Dequeue a -> Dequeue a
x |> (Dequeue f fs b bs) = rebalance $ Dequeue (x : f) (fs + 1) b bs
infixr 5 |>
pop :: Dequeue a -> Maybe (a, Dequeue a)
pop (Dequeue [] _ [] _)      = Nothing
pop (Dequeue [] _ [b] _)       = Just (b, empty)
pop (Dequeue (f : ff) fs b bs) = Just (f, rebalance $ Dequeue ff (fs - 1) b bs)
pop _ = error "Should never have happened"
dequeue :: Dequeue a -> Maybe (a, Dequeue a)
dequeue (Dequeue [] _ [] _)      = Nothing
dequeue (Dequeue [b] _ [] _)       = Just (b, empty)
dequeue (Dequeue f fs (b : bb) bs) = Just (b, rebalance $ Dequeue f fs bb (bs - 1))


instance Foldable Dequeue where
  foldMap fm (Dequeue f _ b _) = foldMap fm f <> foldMap fm (reverse b)
--instance Traversable Dequeue where
--  traverse f d = frontOnly <$> traverse f (toList d)
--
--flatten :: Foldable f => f (Dequeue b) -> Dequeue b
--flatten = frontOnly . concatMap toList
--instance Functor Dequeue where
--  fmap fm (Dequeue f b) = Dequeue (fm <$> f) (fm <$> b)
--instance Applicative Dequeue where
--  pure x = Dequeue [x] []
--  liftA2 f d1 d2 = Dequeue (liftA2 f (toList d1) (toList d2)) []
--instance Monad Dequeue where
--  d >>= f = flatten $ f <$> toList d

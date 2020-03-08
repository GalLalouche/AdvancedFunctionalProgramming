module DS.DequeNaive where

import           Control.Applicative (liftA2)
import           Data.Foldable       (toList)

data Deque a = Deque {front :: [a], back :: [a]}
empty :: Deque a
empty = Deque [] []

frontOnly = flip Deque []

pushr :: a -> Deque a -> Deque a
pushr x (Deque l r) = Deque l (x : r)

pushl :: a -> Deque a -> Deque a
pushl x (Deque l r) = Deque (x : l) r

popl :: Deque a -> Maybe (a, Deque a)
popl (Deque [] [])      = Nothing
popl (Deque [] r)       = popl $ Deque (reverse r) []
popl (Deque (l : ls) r) = Just (l, Deque ls r)

popr :: Deque a -> Maybe (a, Deque a)
popr (Deque [] [])      = Nothing
popr (Deque l [])       = popr $ Deque [] (reverse l)
popr (Deque l (r : rs)) = Just (r, Deque l rs)

instance Foldable Deque where
  foldMap fm (Deque f b) = foldMap fm f <> foldMap fm (reverse b)
instance Traversable Deque where
  traverse f d = frontOnly <$> traverse f (toList d)

flatten :: Foldable f => f (Deque b) -> Deque b
flatten = frontOnly . concatMap toList
instance Functor Deque where
  fmap fm (Deque f b) = Deque (fm <$> f) (fm <$> b)
instance Applicative Deque where
  pure x = Deque [x] []
  liftA2 f d1 d2 = Deque (liftA2 f (toList d1) (toList d2)) []
instance Monad Deque where
  d >>= f = flatten $ f <$> toList d

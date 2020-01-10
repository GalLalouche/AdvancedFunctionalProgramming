{-# LANGUAGE FlexibleInstances #-}
module MySemigroup where
import MyOrd

import Prelude hiding (Semigroup(..), Monoid(..), Ord(..))
class Semigroup a where
  (<>) :: a -> a -> a
class Semigroup a => Monoid a where
  mempty :: a

newtype Sum a = Sum { getSum :: a } deriving Show
instance Num a => Semigroup (Sum a) where
  Sum x <> Sum y = Sum $ x + y
instance Num a => Monoid (Sum a) where
  mempty = Sum 0

newtype Product a = Product a deriving Show
instance Num a => Semigroup (Product a) where
  Product x <> Product y = Product $ x * y
instance Num a => Monoid (Product a) where
  mempty = Product 1

newtype All = All { getAll :: Bool }
instance Semigroup All where
  All x <> All y = All $ x && y
instance Monoid All where
  mempty = All True
newtype Any = Any { getAny :: Bool }
instance Semigroup Any where
  (Any x) <> (Any y) = Any $ x || y
instance Monoid Any where
  mempty = Any False
-- We can define a Semigroup for Maybes in two (exclusive) ways
-- The trivial, or left-biased
instance Semigroup (Maybe a) where
  Nothing <> a = a
  a <> _ = a -- Just 1 <> Just 2 == Just 1
-- The inductive (used by the standard library)
--instance Semigroup a => Semigroup (Maybe a) where
--  Nothing <> a = a
--  a <> Nothing = a
--  (Just x) <> (Just y) = x <> y

--instance Semigroup (a -> a) where
--  (<>) = flip (.)
instance Semigroup b => Semigroup (a -> b) where
  f <> g = \a -> f a <> g a

newtype First a = First (Maybe a)
newtype Last a  = Last (Maybe a)
instance Semigroup (First a) where
  First Nothing <> a = a
  a <> _             = a
instance Semigroup (Last a) where
  a <> Last Nothing = a
  _ <> a            = a

instance Semigroup [a] where
  (<>) = (++)
instance Monoid [a] where
  mempty = []

sconcat :: Semigroup a => [a] -> Maybe a
sconcat = foldr ((<>) . Just) Nothing
mconcat :: Monoid a => [a] -> a
mconcat = foldr (<>) mempty

-- Short for Endomorphism, a functions from some 
-- mathematical object (Set, Class, Category) to itself
newtype Endo a = Endo (a -> a)
appEndo (Endo f) = f 
instance Semigroup (Endo a) where
  (Endo f) <> (Endo g) = Endo $ g . f
instance Monoid (Endo a) where
  mempty = Endo id

newtype Min a = Min { getMin :: a }
instance Ord a => Semigroup (Min a) where
  (Min x) <> (Min y) = Min $ min x y
newtype Max a = Max { getMax :: a }
instance Ord a => Semigroup (Max a) where
  (Max x) <> (Max y) = Max $ max x y

{-# LANGUAGE FlexibleInstances #-}
module MySemigroup where

import Prelude hiding (Semigroup(..), Monoid(..))
class Semigroup a where
  (<>) :: a -> a -> a

instance Semigroup [a] where
  (<>) = (++)

class Semigroup a => Monoid a where
  mempty :: a

newtype Sum a = Sum a deriving Show
instance Num a => Semigroup (Sum a) where
  Sum x <> Sum y = Sum $ x + y
instance Num a => Monoid (Sum a) where
  mempty = Sum 0

newtype Product a = Product a deriving Show
instance Num a => Semigroup (Product a) where
  Product x <> Product y = Product $ x * y
instance Num a => Monoid (Product a) where
  mempty = Product 1

newtype All = All Bool
instance Semigroup All where
  All x <> All y = All $ x && y
newtype Any = Any Bool
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

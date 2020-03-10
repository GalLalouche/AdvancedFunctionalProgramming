{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeOperators     #-}
module MyEq where

import           Data.Char    (ord)
import           Prelude      hiding (Eq, (!=), (==))

import           GHC.Generics ((:*:) (..), (:+:) (..), Generic (..), U1 (..), M1(..), K1(..))

class Eq a where
  (==) :: a -> a -> Bool
  default (==) :: (Generic a, GEq (Rep a)) => a -> a -> Bool
  (==) x y = geq (from x) (from y)

(!=) :: Eq a => a -> a -> Bool
x != y = not (x == y)


instance Eq Int where
  0 == 0 = True
  _ == 0 = False
  x == y = (x - y) == 0

instance Eq Char where
  x == y = ord x == ord y

instance Eq a => Eq [a] where
  [] == [] = True
  _ == [] = False
  [] == _ = False
  (x : xs) == (y : ys) = x == y && xs == ys

class GEq f where
  geq :: f a -> f a -> Bool

instance GEq U1 where
  geq x y = True -- An enum value is always equal to itself
instance (GEq a) => GEq (M1 c m a) where
  geq (M1 x) (M1 y) = geq x y
data Singleton = Singleton deriving Generic
instance Eq Singleton

instance (GEq a, GEq b) => GEq (a :*: b) where
  geq (a :*: b) (c :*: d) = geq a c && geq b d
instance (Eq a) => GEq (K1 i a) where
  geq (K1 x) (K1 y) = x == y

data TwoInts = TwoInts Int Int deriving Generic
instance Eq TwoInts

--data Prod = Prod Int [Int] deriving (Generic, Show)
--instance Eq Prod

instance (GEq a, GEq b) => GEq (a :+: b) where
  geq (L1 x) (L1 y) = geq x y
  geq (R1 x) (R1 y) = geq x y
  geq _ _           = False

data Color = Red | Green | Blue deriving Generic
instance Eq Color

instance Eq a => Eq (Maybe a)


--instance (GEq a, GEq b) => GEq (a :+: b)
--  geq x y = True -- An enum value is always equal to itself

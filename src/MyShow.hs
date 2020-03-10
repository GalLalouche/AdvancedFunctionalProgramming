{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TypeSynonymInstances     #-}

module MyShow where

import           GHC.Generics
import           Prelude      hiding (Show, show)

class Show a where
  show :: a -> String
  default show :: (Generic a, GShow (Rep a)) => a -> String
  show = gshow . from

instance Show Int where
  show 0 = "0"
  show 1 = "1"
  show 2 = "2"
  show 3 = "3"
  show 4 = "4"
  show 5 = "5"
  show 6 = "6"
  show 7 = "7"
  show 8 = "8"
  show 9 = "9"
  show x = if x < 0 then "-" ++ show x else show (x `div` 10) ++ show (x `mod` 10)

instance Show Char where
  show x = [x]

instance Show a => Show (Maybe a) where
  show Nothing  = "Nothing"
  show (Just x) = "Just " ++ show x
instance Show a => Show [a] where
  show []       = ""
  show (x : xs) = show x ++ show xs

class GShow f where
  gshow :: f a -> String
--
--class Show1 f where
--  show1 :: f a -> String -> String

instance GShow U1 where
  gshow U1 = ""

instance (GShow a, Constructor c) => GShow (M1 C c a) where
  gshow c@(M1 a) = conName c ++ "(" ++ gshow a ++ ")"
instance (GShow a, Selector c) => GShow (M1 S c a) where
  gshow s@(M1 a) = selName s ++ "(" ++ gshow a ++ ")"

instance GShow a => GShow (M1 D c a) where
  gshow (M1 x) = gshow x

data Singleton = Singleton deriving Generic
instance Show Singleton

instance (GShow a, GShow b) => GShow (a :+: b) where
  gshow (L1 x) = gshow x
  gshow (R1 x) = gshow x
data Color = Red | Green | Blue deriving Generic
instance Show Color

--instance (GShow a, GShow b) => GShow (a :*: b) where
--  gshow (x :*: y) = gshow x ++ "," ++ gshow y
instance (Show a) => GShow (K1 i a) where
  gshow (K1 x) = show x
data OneInt = OneInt Int deriving Generic
instance Show OneInt

instance (GShow a, GShow b) => GShow (a :*: b) where
  gshow (x :*: y) = gshow x ++ "," ++ gshow y
data TwoInt = TwoInt Int Int deriving Generic
instance Show TwoInt

data Person = Person { age :: Int, name :: String } deriving Generic
instance Show Person
--
--instance (GShow a, GShow b) => GShow (a :*: b) where
--  geq (a :*: b) (c :*: d) = geq a c && geq b d
--instance (Show a) => GShow (K1 i a) where
--  geq (K1 x) (K1 y) = x == y
--
--data TwoInts = TwoInts Int Int deriving Generic
--instance Show TwoInts
--
----data Prod = Prod Int [Int] deriving (Generic, Show)
----instance Show Prod
--
--instance (GShow a, GShow b) => GShow (a :+: b) where
--  geq (L1 x) (L1 y) = geq x y
--  geq (R1 x) (R1 y) = geq x y
--  geq _ _           = False
--
--data Color = Red | Green | Blue deriving Generic
--instance Show Color

--instance Show a => Show (Maybe a)

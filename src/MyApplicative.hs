{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module MyApplicative where

import Prelude hiding (Functor(..), Applicative(..), (<$>), Monoid(..), Monoid(..), Semigroup(..), void)
import MyFunctor
import MySemigroup

class Functor f => Applicative f where
  pure :: a -> f a
  liftA2 :: (a -> b -> c) -> f a -> f b -> f c
  liftA2 f x y = f <$> x <*> y
  (<*>) :: f (a -> b) -> f a -> f b
  (<*>) = liftA2 id
  {-# MINIMAL (pure | liftA2), pure #-}

(*>) :: Applicative f => f a -> f b -> f b
(*>) = liftA2 (\a b -> b)
(<*) :: Applicative f => f a -> f b -> f a
(<*) = liftA2 const

(*>?) :: Applicative f => f a -> f b -> f b
(*>?) fa fb = fb
(<*?) :: Applicative f => f a -> f b -> f a
(<*?) fa fb = fa

instance Applicative Maybe where
  pure = Just
  liftA2 f (Just x) (Just y) = Just $ f x y
  liftA2 _ _ _ = Nothing

instance Applicative ((->) r ) where
  pure = const
  liftA2 f r1 r2 r = f (r1 r) (r2 r)

instance Monoid w => Applicative ((,) w) where
  pure x = (mempty, x)
  liftA2 f (wa, a) (w2, b) = (wa <> w2, f a b)
  
sumMaybes :: Maybe Int -> Maybe Int -> Maybe Int
sumMaybes = liftA2 (+)

sumApplicatives :: Applicative f => f Int -> f Int -> f Int
sumApplicatives = liftA2 (+)
semiSum :: (Applicative f, Semigroup s) => f s -> f s -> f s
semiSum = liftA2 (<>)
instance Applicative (Either a) where
  pure = Right
  liftA2 f (Right x) (Right y) = Right $ f x y
  liftA2 _ (Left l) _ = Left l
  liftA2 _ _ (Left l) = Left l

data Person = Person String Int [Person]
instance Applicative IO where
  liftA2 = undefined
  pure = undefined
getName :: IO String
getName = undefined
getAge :: IO Int
getAge = undefined
getChildren :: IO [Person]
getChildren = undefined
getAddress :: IO String
getAddress = undefined
getPerson :: IO Person
getPerson = Person <$> getName <*> getAge <*> getChildren
getPersonSlow = let
    personCtor = Person ::
      String -> Int -> [Person] -> Person
    fmappedCtor = (personCtor <$>) ::
      IO String -> IO (Int -> [Person] -> Person)
    personWithName = fmappedCtor getName ::
      IO (Int -> [Person] -> Person)
    appliedPersonWithName = (personWithName <*>) ::
      IO Int -> IO ([Person] -> Person)
    personWithNameAndAge = appliedPersonWithName getAge ::
      IO ([Person] -> Person)
    appliedPersonWithNameAndAge = (personWithNameAndAge <*>) ::
      IO [Person] -> IO Person
  in appliedPersonWithNameAndAge getChildren
getPersonWithLiftA2 = liftA2 id (liftA2 Person getName getAge) getChildren
(.:.) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(.:.) = undefined
liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 = liftA2 id .:. liftA2

when :: Applicative f => Bool -> f a -> f ()
when b f = if b then void f else pure ()

traverse_ :: Applicative f => (a -> f b) -> [a] -> f ()
traverse_ f = foldr ((*>) . f) (pure ())

instance Applicative [] where    
  pure x = [x]
  liftA2 f xs ys = [f x y | x <- xs, y <- ys]
newtype ZipList a = ZipList [a]
instance Functor ZipList where
  fmap f (ZipList xs) = ZipList $ fmap f xs
instance Applicative ZipList where
  pure x = ZipList [x]
  liftA2 f (ZipList xs) (ZipList ys) = ZipList $ zipWith f xs ys


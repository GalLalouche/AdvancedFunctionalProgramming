{-# LANGUAGE InstanceSigs #-}
module MyFunctor where

import Prelude hiding (Functor(..))
import MyTree

class Functor f where
  fmap :: (a -> b) -> f a -> f b

(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap

instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just x) = Just $ f x
  
instance Functor (Either a) where
  fmap f (Right a) = Right $ f a 
  fmap _ (Left a) = Left a

instance Functor [] where
  fmap = map

newtype Box a = Box {getBox :: a}
instance Functor Box where
  fmap f (Box a) = Box $ f a

instance Functor Tree where
  fmap _ EmptyTree = EmptyTree
  fmap f (Tree a left right) = Tree (f a) (fmap f left) (fmap f right)


-- Reads some r and returns a
newtype SimpleReader r a = SimpleReader { runReader :: r -> a}
instance Functor (SimpleReader r) where
  -- The new Reader reads some r and returns b
  fmap f sr = SimpleReader $ f . runReader sr


instance Functor ((->) r) where
  fmap = (.)
instance Functor ((,) w) where
  fmap f (w, a) = (w, f a)

fproduct :: Functor f => (a -> b) -> f a -> f (a, b)
fproduct f = fmap (\a -> (a, f a))
square :: Functor f => f a -> f (a, a)
square = fproduct id
strengthen :: Functor f => b -> f a -> f (a, b)
strengthen = fproduct . const
(<$) :: Functor f => b -> f a -> f b
(<$) = fmap . const
void :: Functor f => f a -> f ()
void = (<$) ()

class Contravariant f where
  contramap :: (b -> a) -> f a -> f b

newtype InvFun a b = InvFun (b -> a)
instance Contravariant (InvFun a) where
  contramap f (InvFun g) = InvFun $ g . f

foo :: Maybe Bool -> Maybe (Bool -> Bool)
foo = fmap (&&)

data EitherFunction i j a = FromI (i -> a) | FromJ (j -> a)
instance Functor (EitherFunction i j) where
  fmap f (FromI g) = FromI (f . g)
  fmap f (FromJ g) = FromJ (f . g)
  
instance Functor IO where
  fmap = undefined


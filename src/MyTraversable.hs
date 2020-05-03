{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module MyTraversable where

import Prelude hiding (Traversable(..), Foldable(..), Functor(..), Applicative(..))
import MyFoldable
import MyFunctor
import MyApplicative
import Data.Composition

class (Functor t, Foldable t) => Traversable t where
  sequence :: Applicative f => t (f a) -> f (t a)
  sequence = traverse id
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  traverse = sequence .: fmap
  {-# MINIMAL sequence | traverse #-}

instance Applicative Box where
  pure = Box
  liftA2 f (Box a) (Box b) = Box $ f a b

--instance Traversable t => Foldable t where
--  fmap f fa =
--instance Traversable [] where
--  sequence = foldr (liftA2 (:)) (pure [])

--instance Traversable Maybe where
--  sequence Nothing = pure Nothing
--  sequence (Just x) = fmap Just x

for t = flip traverse t

traverse_ f = void . traverse f

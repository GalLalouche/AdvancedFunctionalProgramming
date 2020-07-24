module MyCont where

import Control.Applicative (liftA2)
import Data.Composition
import Data.Function ((&))

newtype Cont r a = Cont ((a -> r) -> r)


-- TODO teach after contraviarant functors?
instance Functor (Cont r) where
  -- Option1: Domino
  fmap a2b (Cont a2r2r) = Cont $ \b2r -> a2r2r $ b2r . a2b
  -- Option2: step by step
--  fmap a2b (Cont a2r2r) = Cont $ \b2r -> a2r2r $ \a -> b2r $ a2b a

instance Applicative (Cont r) where
  pure = Cont . (&)
  -- Option2: step by step
  (Cont a2b2r2r) <*> (Cont a2r2r) = Cont $ \b2r -> a2b2r2r $ \a2b -> a2r2r $ \a -> b2r (a2b a)
  -- LiftA2: Slow
  liftA2 a2b2c (Cont a2r2r) (Cont b2r2r) = Cont $ \c2r -> a2r2r $ \a -> b2r2r $ \b -> c2r $ a2b2c a b

instance Monad (Cont r) where
  (Cont a2r2r) >>= a2Cont = Cont $ \b2r -> a2r2r $ \a -> let (Cont b2r2r) = a2Cont a in b2r2r b2r


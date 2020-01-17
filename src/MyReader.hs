{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module MyReader where

import Data.Composition
import Control.Applicative


class Monad m => Reader r m where
  local :: (r -> r) -> m a -> m a
  ask :: m r
  ask = reader id
  reader :: (r -> a) -> m a
  reader = flip fmap ask
  {-# MINIMAL (ask | reader), local #-}

instance Reader r ((->) r) where
  ask :: (->) r r
  ask = id
  local = flip (.)
  reader = id

--ask :: (r -> r)
--ask = id
--
--local :: (r -> r) -> (r -> a) -> (r -> a)
--local = flip (.)
--
--reader :: (r -> a) -> r -> a
--reader = id


greet s = "Hello " ++ s ++ "!"
dismiss s = "Goodbye " ++ s ++ "!"

fmap2 :: (a -> b) -> (->) r a -> (->) r b
fmap2 = undefined

greetings = do
  name <- ask
  g <- greet
  d <- dismiss
  return ["I am interacting with " ++ name, g, d]

toUpper :: String -> String
toUpper = undefined
interactScreamName = local toUpper greetings

main = print $ interactScreamName "hi"

----
----instance Functor (Reader r) where
----  fmap :: (a -> b) -> Reader r a -> Reader r b
----  fmap f = Reader . (f .: runReader)
----
----instance Applicative (Reader r) where
----  pure = Reader . const
----  liftA2 f (Reader r1) (Reader r2) = Reader $ \r -> f (r1 r) (r2 r)
----
----instance Monad (Reader r) where
--  r >>= f = Reader $ \x -> runReader (f (runReader r x)) x



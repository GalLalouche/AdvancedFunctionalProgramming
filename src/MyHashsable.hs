{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module MyHashsable where

import qualified Data.Char    as Char
import           GHC.Generics ((:*:) (..), (:+:) (..), Generic (..), K1 (..),
                               M1 (..), Rep (..), U1 (..))
class Hash a where
  hash :: a -> Int
  default hash :: (Generic a, GHash (Rep a)) => a -> Int
  hash = ghash . from

instance Hash Int where
  hash = id
instance Hash Char where
  hash = Char.ord
-- Rest of primitives

instance Hash a => Hash [a] where
  hash []       = 0
  hash (x : xs) = hash x * 31 + hash xs

class GHash f where
  ghash :: f a -> Int

data Singleton = Singleton deriving Generic
instance GHash U1 where
  ghash = const 0
instance (GHash a) => GHash (M1 c m a) where
  ghash (M1 x) = ghash x
instance Hash Singleton

data Ctor = Ctor String deriving Generic
instance (Hash a) => GHash (K1 i a) where
  ghash (K1 x) = hash x
instance Hash Ctor

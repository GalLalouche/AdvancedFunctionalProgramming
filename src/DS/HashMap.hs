{-# LANGUAGE ScopedTypeVariables #-}

module DS.HashMap where

import           Data.Function      ((&))
import           DS.ResizableVector (ResizableVector)
import qualified DS.ResizableVector as V
import           DS.Stack           (Stack)
import qualified DS.Stack           as S

class Eq a => Hashable a where
  hash :: a -> Int
  
data HashMap k v = HashMap {
    table :: ResizableVector (Stack (k, v))
  , size  :: Int
  } deriving Show

_empty ::  Int -> HashMap k v
_empty n = HashMap (go n V.empty) 0 where
  go 0 v = v
  go n v = go (n - 1) (V.pushr (S.empty :: Stack (k, v)) v)

empty = _empty 11

add :: Hashable k => k -> v -> HashMap k v -> HashMap k v
add k v map = let
    t = table map
    h = hash k `mod` V.length t
    existing = table map V.!! h
  in map {table = V.update h (S.push (k, v) existing) t, size = size map + 1}
  
toList :: HashMap k v -> [(k, v)]
toList = concatMap S.toList . V.toList . table

get :: Hashable k => k -> HashMap k v -> Maybe v
get k map = let
    t = table map
    h = hash k `mod` V.length t
    stack = S.toList $ table map V.!! h
  in snd <$> find ((==) k . fst) stack where
    find :: (a -> Bool) -> [a] -> Maybe a
    find _ []       = undefined
    find p (x : xs) = if p x then Just x else find p xs


{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module DS.ResizableVector where

import           Data.Function ((&))
import           Data.Vector   (Vector)
import qualified Data.Vector   as V

import           Debug.Trace

import           Prelude       hiding (length, lookup)

baseSize = 4
data ResizableVector a =
    Leafs {v :: Vector a}
  | Node {h :: Int, l :: Int, vs :: Vector (ResizableVector a)} deriving Show

length :: ResizableVector a -> Int
length (Leafs {v}) = V.length v
length (Node {l})  = l

height :: ResizableVector a -> Int
height (Leafs {}) = 1
height (Node {h}) = h

isFull :: ResizableVector a -> Bool
isFull v = length v >= baseSize ^ height v
empty :: ResizableVector a
empty = Leafs V.empty
pushr :: a -> ResizableVector a -> ResizableVector a
pushr e v = if isFull v
  then Node {h = height v + 1
           , l = length v
           , vs = V.fromList [v]} & pushr e
  else case v of
    Leafs {v} -> Leafs $ V.snoc v e
    Node {..} -> let
      createNewBranch = V.null vs || isFull (V.last vs)
      nextChild = if h == 2
        then empty
        else Node {h = h - 1, l = 0, vs=V.empty}
      updatedVector = if createNewBranch
        then vs `V.snoc` (nextChild & pushr e)
        else vs V.// [(V.length vs - 1, V.last vs & pushr e)]
      in Node {h, l = l + 1, vs = updatedVector}

toList :: ResizableVector a -> [a]
toList (Leafs {v}) = V.toList v
toList (Node {vs}) = concatMap toList vs

-- data is almost always better than a tuple.
data TranslatedIndex a = TranslatedIndex {
  updatedN :: Int,
  index    :: Int,
  branch   :: ResizableVector a}
translateIndex :: Int -> ResizableVector a -> TranslatedIndex a
translateIndex n (Node {h, vs}) = let
  branchSize = baseSize ^ (h - 1)
  updatedN = n `mod` branchSize
  index = n `div` branchSize
  branch = vs V.! index
  in TranslatedIndex {branch, index, updatedN}

lookup :: Int -> ResizableVector a -> Maybe a
lookup n v = if n < 0 || n >= length v
  then Nothing
  else case v of
    Leafs {v} -> Just $ v V.! n -- We already checked bounds.
    Node {h, vs} -> let
      TranslatedIndex {updatedN, branch} = translateIndex n v
      in lookup updatedN branch
      
(!!) :: ResizableVector a -> Int -> a
(!!) v i = case lookup i v of Just x -> x
update :: Int -> a -> ResizableVector a -> ResizableVector a
update n e v = if n < 0 || n >= length v
  then v -- Could also raise an error, or return Nothing
  else case v of
    Leafs {v} -> Leafs $ v V.// [(n, e)]
    Node {..} -> let
      TranslatedIndex {updatedN, branch, index} =
        translateIndex n v
      updatedBranch = update updatedN e branch
      in Node {vs = vs V.// [(index, updatedBranch)], ..}

fromList :: [a] -> ResizableVector a
fromList = foldr pushr empty . reverse

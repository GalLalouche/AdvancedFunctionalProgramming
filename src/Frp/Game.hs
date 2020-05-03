module Frp.Game where

import Data.Vector (Vector, (!), (//))
import Control.Monad (guard)
import Data.List (intercalate)
import Data.Foldable (toList)
import qualified Data.Vector as V

type Grid = Vector (Vector Bool)

empty :: Int -> Grid
empty n = V.replicate n $ V.replicate n False

fromList :: Int -> [(Int, Int)] -> Grid
fromList = foldr (\(j, i) v -> v // [(i, v ! i // [(j, True)])]) . empty

step :: Grid -> Grid
step grid = deepMap next stateAndNeighbors where
  stateAndNeighbors :: Vector (Vector (Bool, Int))
  stateAndNeighbors = let
      width = V.length grid
      height = V.length $ V.head grid
      indexedStates = fmap (\i -> fmap (\j -> (grid ! i ! j, i, j)) js) is where
        is = V.enumFromN 0 width
        js = V.enumFromN 0 height
      countNeighbors (b, i, j) = (b, length [
        () | x <- [i - 1 .. i + 1],
             y <- [j - 1 .. j + 1],
             x /= i || y /= j,
             x >= 0, y >= 0, x < width, y < height,
             grid ! x ! y])
    in deepMap countNeighbors indexedStates
  next :: (Bool, Int) -> Bool
  next (True,  n) = n >= 2 && n <= 3
  next (False, n) = n == 3
  deepMap = fmap . fmap

livePoints :: Grid -> [(Int, Int)]
livePoints grid = [
  (x, y) | y <- [0 .. V.length grid - 1],
           x <- [0 .. V.length (V.head grid) - 1],
           grid ! x ! y]
toString :: Grid -> String
toString = intercalate "\n" . toList . fmap go where
  go :: Vector Bool -> String
  go = toList . fmap (\b -> if b then '*' else '-')

{-# LANGUAGE BangPatterns #-}
module DS.Queue where

import Prelude as P
data Queue a = Queue ![a] ![a]

enqueue :: a -> Queue a -> Queue a
enqueue !x (Queue l r) = Queue (x : l) r

dequeue :: Queue a -> Maybe (a, Queue a)
dequeue (Queue [] [])      = Nothing
dequeue (Queue l  [])      = dequeue (Queue [] (reverse l))
dequeue (Queue l (r : rs)) = Just (r, Queue l rs)

peek :: Queue a -> Maybe a
peek = go . dequeue where
  go (Just (x, _)) = Just x
  go Nothing       = Nothing
  
length :: Queue a -> Int
length (Queue l r) = P.length l + P.length r


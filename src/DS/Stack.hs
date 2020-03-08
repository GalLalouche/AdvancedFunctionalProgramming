module DS.Stack(
  Stack(toList, length), -- No constructor is provided
  empty, push, pop, (++), reverse, null
) where

import Prelude hiding ((++), reverse, null)
import qualified Prelude as P

data Stack a = Stack {toList :: [a], length :: Int}
  deriving Show

empty :: Stack a
empty = Stack [] 0

push :: a -> Stack a -> Stack a
push x (Stack xs l) = Stack (x : xs) (l + 1)

pop :: Stack a -> Maybe (a, Stack a)
pop (Stack xs l) = case xs of
  [] -> Nothing
  (x : xs) -> Just (x, Stack xs (l - 1))

null = P.null . toList

(++) :: Stack a -> Stack a -> Stack a
(Stack s1 l1) ++ (Stack s2 l2) = Stack (s1 P.++ s2) (l1 + l2)

reverse :: Stack a -> Stack a
reverse (Stack xs l) = Stack (P.reverse xs) l

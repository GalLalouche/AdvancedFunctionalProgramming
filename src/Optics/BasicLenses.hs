module Optics.BasicLenses where

import           Control.Monad   ((>=>))
import           Data.Char       (toUpper)
import           Optics.LensLess (Company (..), Person (..))

data Lens s a = Lens { get :: s -> a, set :: a -> s -> s }
modify :: Lens s a -> (a -> a) -> s -> s
modify (Lens get set) f s = set (f $ get s) s
cl :: Lens a b -> Lens b c -> Lens a c
cl (Lens gb sb) (Lens gc sc) = Lens (gc . gb) c2a where
  c2a c a = sb (sc c (gb a)) a

data Traversal s a = Traversal {
    getAll :: s -> [a], modifyAll :: (a -> a) -> s -> s }
setAll :: Traversal s a -> a -> s -> s
setAll t = modifyAll t . const


t2t :: Traversal a b -> Traversal b c -> Traversal a c
t2t t1 t2 = Traversal g m where
  g = getAll t1 >=> getAll t2
  m = modifyAll t1 . modifyAll t2

t2l :: Traversal a b -> Lens b c -> Traversal a c
t2l t l = Traversal g m where
  g = map (get l) . getAll t
  m = modifyAll t . modify l

l2t :: Lens a b -> Traversal b c -> Traversal a c


l2t l t = Traversal g m where
  g = getAll t . get l
  m = modify l . modifyAll t

personsTraversal :: Traversal Company Person
personsTraversal =
    Traversal persons (\ f -> Company . map f . persons)
lastNameTraversal :: Traversal Person Char
lastNameTraversal = Traversal g m where
  g = (!! 1) . words . name
  m f p = let
      names = words $ name p
      firstName = names !! 0
      lastName = names !! 1
    in p { name = unwords [firstName, map f lastName] }
upperCaseLastNames :: Company -> Company
upperCaseLastNames = modifyAll (personsTraversal `t2t` lastNameTraversal) toUpper

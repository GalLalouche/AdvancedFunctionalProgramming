{-# LANGUAGE MonomorphismRestriction #-}
{-# LANGUAGE Rank2Types              #-}
{-# LANGUAGE ScopedTypeVariables     #-}

module Optics.FunctorLenses where

import           Data.Char             (toUpper)
import           Data.Functor.Const    (Const (..))
import           Data.Functor.Identity (Identity (..))
import           Data.Monoid           (Endo (..))

type Lens s a = forall f . Functor f => (a -> f a) -> (s -> f s)
view :: forall s a . Lens s a -> s -> a
view lens = runIdentity . go return where
  go :: (a -> Identity a) -> (s -> Identity a)
  go f = getConst . lens (Const . f)

type Getting r s a = (a -> Const r a) -> s -> Const r s
genView :: Getting a s a -> s -> a
genView l = getConst . l Const

type Setting s a = (a -> Identity a) -> (s -> Identity s)
over :: Setting s a -> (a -> a) -> s -> s
over l f = runIdentity . l (Identity . f)

set :: Setting s a -> a -> s -> s
set lens = over lens . const

compose :: Lens a b -> Lens b c -> Lens a c
compose = (.) -- Really!

(^.) :: s -> Lens s a -> a
(^.) s l = view l s


data Person = Person { _name :: String }
setName :: Person -> String -> Person
setName p name = p { _name = name }
name :: forall f . Functor f => (String -> f String) -> (Person -> f Person)
name f p = let
     n = _name p :: String
     modifiedName = f n :: f String
     setName (name :: String) = p {_name = name} :: Person
     liftedSetName = fmap setName :: f String -> f Person
  in liftedSetName modifiedName

at :: Int -> Lens [a] a
at = undefined
data Company = Company [Person]
persons :: Lens Company [Person]
persons = undefined

upperCaseLastNames :: Company -> Company
upperCaseLastNames = over persons (map go1) where
  go1 :: Person -> Person
  go1 = over lastNameLens (map toUpper)
  lastNameLens :: Lens Person String
  lastNameLens = undefined

foo :: Company -> Char
foo company = company^.(persons.at 2.name.at 0)
type Traversal s a = forall f. Applicative f => (a -> f a) -> (s -> f s)

toListOfSlow :: forall s a . Traversal s a -> s -> [a]
toListOfSlow trav = go return where
  go :: (a -> [] a) -> (s -> [] a)
  go f = getConst . trav (Const . f)

-- This isn't exactly the same signature, but it's needed for
-- ScopedTypeVariables to work correctly.
lensToTraversal :: forall s a f .
    Lens s [a] -> Applicative f => (a -> f a) -> s -> f s
lensToTraversal l f s = let
    traversed = traverse f :: [a] -> f [a]
    traversedGet = traversed . view l :: s -> f [a]
    flippedSet = flip $ set l :: s -> [a] -> s
    fa = traversedGet s :: f [a]
    setter = flippedSet s :: [a] -> s
    liftedSetter = fmap setter :: f [a] -> f s
  in liftedSetter fa
nameTraversal :: forall f. Applicative f => (Char -> f Char) -> (Person -> f Person)
nameTraversal f = let
    traversed = traverse f :: String -> f String
    traversedName = traversed . _name :: Person -> f String
  in fmap Person . traversedName
--toListOf :: forall s a . Traversal s a -> s -> [a]
--toListOf traversal = go where
--  go :: s -> [a]
--  go = (`appEndo` []) . go2
--  go2 = getConst . traversal (Const . Endo . (:))
--

module Optics.LensesMore where

import Optics.BasicLenses (Lens(..), cl, modify)

data RGB = RGB Int Int Int
red :: Lens RGB Int
red = Lens (\ (RGB r _ _) -> r) (\r (RGB _ g b) -> RGB r g b)
at :: Int -> Lens [a] a
at i = Lens (!! i) (go i) where
  go 0 a (_ : xs) = a : xs
  go n a (x : xs) = x : go (n - 1) a xs
data Image = Image [[RGB]]
image :: Lens Image [[RGB]]
image = Lens (\ (Image i) -> i) (\ i (Image _) -> Image i)

updateRedValue :: (Int, Int) -> Int -> Image -> Image
updateRedValue (x, y) = set (image `cl` at x `cl` at y `cl` red)

data Person = Person ()
type PersonId = String
data Contacts = Contacts ()
data Messages = Messages ()
data RecentCalls = RecentCalls ()
data Phone = Phone {
    contacts :: Contacts
  , messages :: Messages
  , recentCalls :: RecentCalls }
contactsPersonLens :: PersonId -> Lens Contacts Person
contactsPersonLens = undefined
messagesPersonLens :: PersonId -> Lens Messages Person
messagesPersonLens = undefined
recentPersonLens :: PersonId -> Lens RecentCalls Person
recentPersonLens = undefined
phonePersonLens :: PersonId -> Lens Phone Person
phonePersonLens pid = Lens getter setter where
  getter (Phone c _ _) = get (contactsPersonLens pid) c
  setter person (Phone c m rc) = let
    uc = update contactsPersonLens c
    um = update messagesPersonLens m
    urc = update recentPersonLens rc
    in Phone uc um urc where
      update :: (PersonId -> Lens s Person) -> s -> s
      update l = set (l pid) person

name :: Lens Person String
name = undefined
rename :: PersonId -> (String -> String) -> Phone -> Phone
rename pid = modify $ phonePersonLens pid `cl` name


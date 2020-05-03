module Optics.SettersAndGetters where

import           Data.Char       (toUpper)
import           Optics.LensLess (Company (..), Person (..))

type Getter s a = s -> a
(^.) :: Getter a b -> Getter b c -> Getter a c
(^.) = flip (.)
infixl 8 ^.

type Setter s a = (a -> a) -> s -> s
cs :: Setter a b -> Setter b c -> Setter a c
cs = (.) -- huh...
set :: Setter s a -> a -> s -> s
set = (. const)

data RGB = RGB { red :: Int, green :: Int, blue :: Int }
setRed :: Setter RGB Int
setRed f rgb = rgb { red = f $ red rgb }
newtype Image = Image [[RGB]]
setAt :: Int -> Setter [a] a
setAt 0 f (x : xs) = f x : xs
setAt n f (x : xs) = x : setAt (n - 1) f xs

setImage :: Setter Image [[RGB]]
setImage f (Image xss) = Image $ f xss
updateRedValue :: (Int, Int) -> Int -> Image -> Image
updateRedValue (x, y) = set $
    setImage `cs` setAt x `cs` setAt y `cs` setRed

setName :: Setter Person String
setName f p = p { name = f $ name p}

setPersons :: Setter Company [Person]
setPersons f c = c { persons = f $ persons c }

deepComposed :: Company -> Company
deepComposed = let
    setter = setPersons `cs` setAt 3 `cs` setName `cs` setAt 0
  in setter toUpper


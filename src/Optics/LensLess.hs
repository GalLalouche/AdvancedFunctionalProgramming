module Optics.LensLess where

import           Data.Char (toUpper)

data Person = Person {
    name    :: String
  , age     :: Int
  , address :: String
  }

incrementAge :: Person -> Person
incrementAge p = p { age = age p + 1}

data Company = Company { persons :: [Person]}
deepModifications :: Company -> Company
deepModifications c = c { persons = go 2 $ persons c } where
  go :: Int -> [Person] -> [Person]
  go 0 (p : ps) = p {name = capitalize $ name p} : ps
  go n (p : ps) = p : go (n - 1) ps
  capitalize :: String -> String
  capitalize (x : xs) = toUpper x : xs

getFirstLetterOfThirdPerson :: Company -> Char
getFirstLetterOfThirdPerson = head . name . (!! 2) . persons


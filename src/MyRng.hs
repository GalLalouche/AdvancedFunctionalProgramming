{-# LANGUAGE TupleSections #-}
module MyRng where

import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Char (ord, chr, toUpper)
import Data.Composition
import Data.Bits (shiftL, shiftR)
import Control.Applicative
import MyTree

type Seed = Int
data RngConfig = RngConfig { multiplier :: Int, increment :: Int}
newtype Rng = Rng (Seed -> Seed)
data Random = Random Rng Seed
instance Show Random where
  show (Random _ seed) = "Random (" ++ show seed ++ ")"

linearCongruentialGenerator :: RngConfig -> Rng
linearCongruentialGenerator (RngConfig m i) = Rng $ \seed -> (seed * m + i) `mod` (2 `shiftL` 32)

nextInt :: Random -> (Int, Random)
nextInt (Random r@(Rng next) s) = let n = next s in (n, Random r n)

gccConfig :: Rng
gccConfig = linearCongruentialGenerator $ RngConfig 1103515245 12345

currentTime :: IO Int
currentTime = fmap round getPOSIXTime
newRng :: Rng -> IO Random
newRng config = fmap (Random config) currentTime
newGcc :: IO Random
newGcc = newRng gccConfig

newtype RandomGen a = RandomGen { runRandom :: Random -> (a, Random) }
instance Functor RandomGen where
  fmap f (RandomGen g) = RandomGen $ \r -> let (a, newR) = g r in (f a, newR)
instance Applicative RandomGen where
  pure x = RandomGen (x,)
  liftA2 f (RandomGen g1) (RandomGen g2) =
    RandomGen $ \r -> let
      (a, newR1) = g1 r
      (b, newR2) = g2 newR1
    in (f a b, newR2)
instance Monad RandomGen where
  (RandomGen g) >>= f = RandomGen $ \r -> let
      (a, newR) = g r
      (RandomGen g2) = f a
    in g2 newR

class Randomable a where
  gen :: RandomGen a
  shrink :: a -> [a]
  shrink _ = []
instance Randomable Int where
  gen = do
    x <- RandomGen nextInt
    y <- RandomGen nextInt
    return $ (x `shiftL` 15) + (y `shiftR` 16)
  shrink = reverse . aux where
    aux 0 = []
    aux n = let d = n `div` 2 in d : aux d

genInt :: RandomGen Int
genInt = gen
genMax :: Int -> RandomGen Int
genMax n = do
  i <- gen
  return $ i `mod` n
-- Taking the LSB is A Bad Idea
instance Randomable Bool where
  gen = liftA2 (<) genInt genInt
instance Randomable a => Randomable (Maybe a) where
  gen = do
    isNothing <- gen
    if isNothing then return Nothing else Just <$> gen
genSimple :: RandomGen Int
genSimple = gen
--  x <- gen
--  unused <- gen :: RandomGen Int
--  return x
genList :: Randomable a => Int -> RandomGen [a]
genList 0 = return []
genList n = do
  x <- gen
  rest <- genList (n - 1)
  return $ x : rest

genIntMax :: Int -> RandomGen Int
genIntMax n = fmap (`mod` n) gen

genAlphaChar :: RandomGen Char
genAlphaChar = do
  i <- genIntMax 26
  let char = chr $ asciiForA + i
  isUpperCase <- gen
  return $ if isUpperCase then toUpper char else char
  where asciiForA = ord 'a'

genListOfSize :: RandomGen a -> Int -> RandomGen [a]
genListOfSize _  0 = return []
genListOfSize rg n = liftA2 (:) rg (genListOfSize rg (n - 1))

genListOfMaxSize :: RandomGen a -> Int -> RandomGen [a]
genListOfMaxSize _ 0 = return []
genListOfMaxSize rg n = genIntMax n >>= genListOfSize rg

genAlphaString :: Int -> RandomGen String
genAlphaString = genListOfMaxSize genAlphaChar

data Person = Person { name :: String, age :: Int}
instance Randomable Person where
  gen = do
    firstName <- genAlphaString 10
    lastName <- genAlphaString 10
    age <- genIntMax 120
    return $ Person (firstName ++ " " ++ lastName) age

instance Randomable a => Randomable [a] where
  gen = do
    isEmpty <- (> 8) <$> genIntMax 11
    if isEmpty then return [] else liftA2 (:) gen gen
  shrink xs = safeInit (inits xs) ++ shrinkOne xs where
    safeInit xs = if null xs then [] else init xs
    -- Exists in Data.List
    inits :: [a] -> [[a]]
    inits [] = [[]]
    inits (x : xs) = [] : fmap (x :) (inits xs)
    shrinkOne [] = []
    shrinkOne (x : xs) =
        [x' : xs | x' <- shrink x] ++
        [x : xs' | xs' <- shrinkOne xs]

instance Randomable a => Randomable (Tree a) where
  gen = do
    isEmpty <- gen
    if isEmpty then return EmptyTree
               else Tree <$> gen <*> gen <*> gen


{-# LANGUAGE TupleSections #-}
module MyRng where

import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Composition
import Data.Bits (shift)
import Control.Applicative
import MyTree

type Seed = Int
data RngConfig = RngConfig { multiplier :: Int, increment :: Int}
newtype Rng = Rng (Seed -> Seed)
data Random = Random Rng Seed
instance Show Random where
  show (Random _ seed) = "Random (" ++ show seed ++ ")"

linearCongruentialGenerator :: RngConfig -> Rng
linearCongruentialGenerator (RngConfig m i) = Rng $ \seed -> (seed * m + i) `mod` (2 `shift` 31)

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
instance Randomable Int where
  gen = RandomGen nextInt
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
genSimple = gen >>= return >>= return
--  x <- gen
--  unused <- gen :: RandomGen Int
--  return x
genList :: Randomable a => Int -> RandomGen [a]
genList 0 = return []
genList n = do
  x <- gen
  rest <- genList (n - 1)
  return $ x : rest
instance Randomable a => Randomable [a] where
  gen = do
    isEmpty <- gen
    if isEmpty then return [] else liftA2 (:) gen gen

instance Randomable a => Randomable (Tree a) where
  gen = do
    isEmpty <- gen
    if isEmpty then return EmptyTree
               else Tree <$> gen <*> gen <*> gen


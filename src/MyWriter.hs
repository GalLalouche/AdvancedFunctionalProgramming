{-# LANGUAGE TupleSections #-}

module MyWriter where

--import           MyApplicative
--import           MyFunctor
--import           MyMonad
import Data.Semigroup

tell :: w -> (w, ())
tell = (, ())

listen :: (w, a) -> (w, (a, w))
listen (w, a) = (w, (a, w))

pass :: (w, (a, w -> w)) -> (w, a)
pass (w, (a, f)) = (f w, a)

listens :: (w -> b) -> (w, a) -> (w, (a, b))
listens f (w, a) = (w, (a, f w))

censor :: (w -> w) -> (w, a) -> (w, a)
censor f (w, a) = (f w, a)

type StringListWriter a = ([String], a)

newtype Expression =
  Expression String
  deriving (Show)

instance Semigroup Expression where
  (Expression e1) <> (Expression e2) = Expression $ "(" ++ e1 ++ ")" ++ e2

instance Monoid Expression where
  mempty = Expression ""

type StringWriter a = (String, a)
add :: Int -> Int -> StringWriter Int
add a x = ("I added " ++ show a ++ ".", x + a)
sub :: Int -> Int -> StringWriter Int
sub a x = ("I subtracted " ++ show a ++ ".", x + a)
mul :: Int -> Int -> StringWriter Int
mul a x = ("I multiplied by " ++ show a ++ ".", x + a)

calcAndTell :: Int -> StringWriter Int
calcAndTell x = do
  tell $ "I got " ++ show x ++ "."
  result <- add 1 x >>= mul 3 >>= sub 5
  tell $ "I finished with " ++ show result ++ "."
  return result

type ListWriter a = ([String], a)
gcdAndTell :: Int -> Int -> ListWriter Int
gcdAndTell x 0 = do
  tell ["Got GCD of " ++ show x]
  return 1
gcdAndTell x y = do
  let m = x `mod` y
  tell [show x ++ " % " ++ show y ++ " = " ++ show m]
  gcdAndTell y m

type Price = Int
type TotalItems = Sum Int
type TotalItemsWriter a = (TotalItems, a)
buy :: Price -> TotalItemsWriter Price
buy = (,) (Sum 1)
thirdItemFree :: Price -> TotalItemsWriter Price
thirdItemFree p = do
  (p, Sum total) <- listen $ buy p
  return $ if total == 1 then 0 else p
--
--add :: Int -> Int -> StringWriter Int
--add a x = (Expression $ " + " ++ show a, x + a)
--
--sub :: Int -> Int -> StringWriter Int
--sub a x = (Expression $ " - " ++ show a, x - a)
--
--mul :: Int -> Int -> StringWriter Int
--mul a x = (Expression $ " * " ++ show a, x * a)


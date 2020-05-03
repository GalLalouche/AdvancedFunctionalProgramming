{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TupleSections #-}
module More where

import Control.Exception.Base (IOException, catch, SomeException)
import Control.Concurrent.Async (Async, cancelWith)
import Data.Char (ord, toLower)
import Data.Composition((.:))
import qualified Data.Char as Data.Char
import Data.Bits (shiftL, shiftR, xor)
import MyRng (Random(..), RandomGen(..), RngConfig(..), linearCongruentialGenerator)

import Data.Monoid (Endo(..))

class Bifunctor f where
  bimap :: (a -> c) -> (b -> d) -> f a b -> f c d
  leftMap :: (a -> c) -> f a b -> f c b
  leftMap = flip bimap id
  rightMap :: (b -> d) -> f a b -> f a d
  rightMap = bimap id

instance Bifunctor Either where
  bimap f _ (Left a) = Left $ f a
  bimap _ g (Right b) = Right $ g b

instance Bifunctor (,) where
  bimap f g (a, b) = (f a, g b)

instance Bifunctor ((,,) a) where
  bimap f g (x, a, b) = (x, f a, g b)

umap :: Bifunctor f => (a -> b) -> f a a -> f b b
umap f = bimap f f

bivoid :: Bifunctor f => f a b -> f () ()
bivoid = let v = const () in bimap v v

data TF f g a b = TF (f a) (g b)
instance (Functor f, Functor g) => Bifunctor (TF f g) where
  bimap f g (TF fa  gb) = TF (fmap f fa) (fmap g gb)


class Contravariant f where
  contramap :: (b -> a) -> f a -> f b
  (>$) :: a -> f a -> f b
  (>$) = contramap . const

newtype Op a b = Op { getOp :: b -> a}
instance Contravariant (Op a) where
  contramap f (Op op) = Op $ op . f

mapInput :: (b -> c) -> (c -> a) -> (b -> a)
mapInput = flip (.)

newtype Predicate a = Predicate {getPredicate :: a -> Bool}
instance Contravariant Predicate where
  contramap f (Predicate op) = Predicate $ op . f

newtype Comparison a = Comparison {
  getComparison :: a -> a -> Ordering }
instance Contravariant Comparison where
  contramap f (Comparison c) = Comparison $ \ x y -> c (f x) (f y)
defaultComparison :: Ord a => Comparison a
defaultComparison = Comparison compare


sort :: Comparison a -> [a] -> [a]
sort = undefined

sortOnContramap :: Ord b => (a -> b) -> [a]  -> [a]
sortOnContramap = sort . flip contramap defaultComparison
data Person = Person { name :: String }
sortByName :: [Person] -> [Person]
sortByName = sortOnContramap name


-- Stands for an equivalence relations.
-- (reflexive, symmetric, transitive)
newtype Equivalence a = Equivalence {
  getEquivalence :: a -> a -> Bool}
instance Contravariant Equivalence where
  contramap f (Equivalence c) = Equivalence $ \ x y -> c (f x) (f y)
data Animal = Animal {numberOfLegs :: Int, speak :: String}
--data Dog = Dog {name :: String}
--upcast :: Dog -> Animal
--upcast d = Animal 4 ("Woof! my name is " ++ name d)
--overrideReturnType :: (a -> Dog) -> (a -> Animal)
--overrideReturnType = fmap upcast
--overrideInput :: (Animal -> a) -> (Dog -> a)
--overrideInput = getOp . contramap upcast . Op
--

class Invariant f where
  invmap :: (a -> b) -> (b -> a) -> f a -> f b


instance Invariant Endo where
  invmap f g (Endo endo) = Endo $ \b -> f $ endo $ g b

data Json = Json ()
data JsonEncoder a = JsonEncoder { fromJson :: Json -> a, toJson :: a -> Json }
instance Invariant JsonEncoder where
  invmap f g (JsonEncoder from to) = JsonEncoder (f . from) (to . g)

data MonoidOp a = MonoidOp { zero :: a, append :: a -> a -> a }

instance Invariant MonoidOp where
  invmap f g (MonoidOp z o) =
   MonoidOp (f z) (\x y -> f $ o (g x) (g y))

-- generates a random value from a given seed.
-- Isomorphic to Endo, of course.

data Codec a = Codec { read :: String -> a, write :: a -> String }
instance Invariant Codec where
  invmap f g (Codec r w) = Codec (f . r) (w . g)
stringCodec :: Codec String
stringCodec = Codec id id
boolCodec :: Codec Bool
boolCodec = invmap s2b b2s stringCodec where
  s2b = (==) "true"
  b2s b = if b then "true" else "false"
ciCodec = invmap lower lower stringCodec where
  lower = map Data.Char.toLower
caseInsensitiveBoolCodec = invmap s2b b2s ciCodec where
  s2b = (==) "true"
  b2s b = if b then "true" else "false"

class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
  -- Applies f a at least once
  some :: f a -> f [a]
  some v = (:) <$> v <*> many v
  -- Applies f a zero or more times
  many :: f a -> f [a]
  many v = some v <|> pure []


-- Some obvious instances
instance Alternative Maybe where
  empty = Nothing
  Nothing <|> r = r
  l <|> _       = l
instance Alternative [] where
  empty = mempty
  (<|>) = (<>)

guard :: Alternative f => Bool -> f ()
guard b = if b then pure () else empty

asum :: (Foldable t, Alternative f) => t (f a) -> f a
asum = foldr (<|>) empty

findFirst :: (a -> Maybe b) -> [a] -> Maybe b
findFirst = asum .: fmap

-- Doesn't introduce any new methods, just new superclasses.
-- The names are, once again, a historical left-over.
class (Monad f, Alternative f) => MonadPlus f where
  mzero :: f a
  mzero = empty
  mplus :: f a -> f a -> f a
  mplus = (<|>)

mfilter :: MonadPlus f => (a -> Bool) -> f a -> f a
mfilter p = ((\x -> if p x then empty else pure x) =<<)

mMapMaybe :: MonadPlus f => (a -> Maybe b) -> f a -> f b
mMapMaybe f = (=<<) (maybe empty return . f)

mfilterM :: forall a m t .
    (Applicative m, MonadPlus t, Traversable t) =>
    (a -> m Bool) -> t a -> m (t a)
mfilterM p = fmap filter . traverse addBool where
  addBool :: a -> m (a, Bool)
  addBool x = fmap (x,) (p x)
  filter :: t (a, Bool) -> t a
  filter = fmap fst . mfilter snd

class Monad m => MonadError m e | m -> e where
  throwError :: e -> m a
  catchError :: (e -> m a) -> m a -> m a

instance MonadError Maybe () where
  throwError _ = Nothing
  catchError f Nothing = f ()
  catchError _ m       = m

instance MonadError (Either e) e where
  throwError = Left
  catchError f (Left l) = f l
  catchError _ r        = r

instance MonadError IO IOException where
  throwError = ioError
  catchError = flip catch

pureCatchError :: MonadError m e => (e -> a) -> m a -> m a
pureCatchError f = catchError (return . f)
toEither :: MonadError m e => m a -> m (Either e a)
toEither = pureCatchError Left . fmap Right
toMaybe :: MonadError m e => m a -> m (Maybe a)
toMaybe = pureCatchError (const Nothing) . fmap Just
orElse :: forall m e a . MonadError m e => m a -> m a -> m a
orElse = catchError . const
orElsePure :: MonadError m e => a -> m a -> m a
orElsePure = orElse . return
fromEither :: MonadError m e => (Either e a -> m b) -> m a -> m b
fromEither f = (f =<<) . toEither
filterError :: MonadError m e => (a -> Maybe e) -> m a -> m a
filterError f = (=<<) (\x -> case f x of
    Nothing -> return x
    Just e  -> throwError e
  )

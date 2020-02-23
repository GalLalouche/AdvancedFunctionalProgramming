{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Rank2Types    #-}
{-# LANGUAGE TypeOperators #-}

module MyRealFree where

import           Control.Applicative (liftA2)
import           Control.Monad       ((>=>))
import           Control.Monad.State
import           Data.Composition
import           Data.Foldable       (traverse_)
import           Data.Function       ((&))
import           Data.Map.Strict     (Map, (!), (!?))
import qualified Data.Map.Strict     as Map
import           Prelude             hiding (readFile, writeFile)
import qualified Prelude             as P (readFile, writeFile)
import qualified System.Directory    as D

data Free f a = Return a | Join (f (Free f a))

instance (Functor f) => Functor (Free f) where
  fmap f (Return a) = Return $ f a
  fmap f (Join fa)  = Join $ fmap f <$> fa
instance (Functor f) => Applicative (Free f) where
  pure = Return
  Return f <*> fb = f <$> fb
  Join fa <*> fb = Join $ (<*> fb) <$> fa
instance (Functor f) => Monad (Free f) where
  Return a >>= f = f a
  Join fa >>= f = Join $ (>>= f) <$> fa

infixr 0 ~>
type f ~> g = forall x. f x -> g x
listToMaybe :: [] ~> Maybe
listToMaybe []       = Nothing
listToMaybe (x : xs) = Just x
listsToMaybesBad ::
    ([a] -> Maybe a) -> [a] -> [b] -> (Maybe a, Maybe b)
listsToMaybesBad = undefined
listsToMaybesGood ::
    ([] ~> Maybe) -> [a] -> [b] -> (Maybe a, Maybe b)
listsToMaybesGood f as bs = (f as, f bs) -- Yey!

interpret :: Monad m => (f ~> m) -> Free f a -> m a
interpret _ (Return a) = return a
interpret f (Join fa)  = f fa >>= foldFree f
foldFree :: Monad m => (f ~> m) -> (Free f ~> m)
foldFree _ (Return a) = return a
foldFree f (Join fa)  = f fa >>= foldFree f

data IOProgramF a = ReadFile FilePath (String -> a) |
                    TempFile String (FilePath -> a) |
                    WriteFile FilePath String a
                    -- Etc.
                    deriving Functor
type IOProgram = Free IOProgramF

liftFunc :: ((a -> Free f a) -> f (Free f a)) -> Free f a
liftFunc = Join . (&) Return
liftUnit :: (Free f () -> f (Free f ())) -> Free f ()
liftUnit = Join . (&) (Return ())
readFile :: FilePath -> IOProgram String
readFile = liftFunc . ReadFile
tempFile :: String -> IOProgram FilePath
tempFile = liftFunc . TempFile
writeFile :: FilePath -> String -> IOProgram ()
writeFile = liftUnit .: WriteFile

anotherProgram :: IOProgram String
anotherProgram = undefined
program :: IOProgram ()
program = do
  file1 <- readFile "file1"
  file2 <- readFile "file2"
  tempFile <- tempFile $ file1 ++ file2
  externalOutput <- anotherProgram
  writeFile "output" externalOutput

runProgramIO :: FilePath -> IOProgram a -> IO a
runProgramIO pwd p = do
  (res, tempFiles) <- runStateT (interpret go p) []
  traverse_ D.removeFile tempFiles
  return res where
    -- Notice the types: an interpreter works on the underlying
    -- IOProgramF, not Free.
    fullPath = ((pwd ++ "/") ++)
    go :: IOProgramF ~> StateT [FilePath] IO
    go (ReadFile path next) =
      next <$> (liftIO $ P.readFile (fullPath path))
    go (WriteFile path contents next) =
      next <$ (liftIO $ P.writeFile (pwd ++ "/" ++ path) contents)
    go (TempFile contents next) = do
      numberOfFiles <- gets length
      let tempFileName = "temp" ++ show numberOfFiles
      let fullPath = pwd ++ "/" ++ tempFileName
      liftIO $ P.writeFile fullPath contents
      modify (fullPath :)
      return $ next fullPath

type Files = Map FilePath String
runProgramFake :: IOProgram a -> (a, Files)
runProgramFake p = runState (interpret stateInterpreter p) Map.empty
stateInterpreter :: IOProgramF ~> State Files
stateInterpreter (ReadFile fp next) = next <$> gets (! fp)
stateInterpreter (WriteFile fp contents next) =
  next <$ modify (Map.insert fp contents)
stateInterpreter (TempFile contents next) = do
  numberOfFiles <- gets Map.size
  let tempFileName = "temp" ++ show numberOfFiles
  modify $ Map.insert tempFileName contents
  return $ next tempFileName

hoistFree :: Functor g => (f ~> g) -> Free f a -> Free g a
hoistFree _ (Return a) = Return a
hoistFree f (Join fa) = Join $ hoistFree f <$> f fa

stateToIO :: State Files ~> IO
stateToIO = do
  files <- 
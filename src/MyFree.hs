{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
module MyFree where

import Control.Applicative (liftA2)
import Data.Map.Strict (Map, (!?), (!))
import qualified Data.Map.Strict as Map
import Data.Foldable (traverse_)
import Control.Monad.State
import Prelude hiding (readFile, writeFile)
import qualified Prelude as P (readFile, writeFile)
import qualified System.Directory as D
import Control.Monad((>=>))

data Free f a = Return a | Join (f (Free f a))

instance (Functor f) => Functor (Free f) where
  fmap f (Return a) = Return $ f a
  fmap f (Join fa) = Join $ fmap f <$> fa
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
listToMaybe [] = Nothing
listToMaybe (x : xs) = Just x
listsToMaybesBad :: 
    ([a] -> Maybe a) -> [a] -> [b] -> (Maybe a, Maybe b)
listsToMaybesBad = undefined
listsToMaybesGood :: 
    ([] ~> Maybe) -> [a] -> [b] -> (Maybe a, Maybe b)
listsToMaybesGood f as bs = (f as, f bs) -- Yey!

foldFree :: Monad m => (f ~> m) -> (Free f ~> m)
foldFree _ (Return a) = return a
foldFree f (Join fa) = f fa >>= foldFree f

data IOProgram a = EmptyProgram a |
                   ReadFile FilePath (String -> IOProgram a) |
                   Exists FilePath (Bool -> IOProgram a)     |
                   TempFile String (FilePath -> IOProgram a) |
                   WriteFile FilePath String (IOProgram a)   |
                   DeleteFile FilePath (IOProgram a)
                   deriving Functor

instance Monad IOProgram where
  EmptyProgram a >>= fb = fb a
  ReadFile fp next >>= fb = ReadFile fp (next >=> fb)
  Exists fp next >>= fb = Exists fp (next >=> fb)
  WriteFile fp contents next >>= fb = WriteFile fp contents (next >>= fb)
  -- Etc.

instance Applicative IOProgram where
  pure = EmptyProgram
  -- No reason to work tedious work twice.
  fa <*> fb = fa >>= (<$> fb)

readFile :: FilePath -> IOProgram String
readFile fp = ReadFile fp EmptyProgram
tempFile :: String -> IOProgram FilePath
tempFile contents = TempFile contents EmptyProgram
writeFile :: FilePath -> String -> IOProgram ()
writeFile fp contents = WriteFile fp contents (EmptyProgram ())
-- etc.

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
  (res, tempFiles) <- runStateT (go pwd p) []
  traverse_ D.removeFile tempFiles
  return res where
    -- Maintain a list of all temporary files.
    go :: FilePath -> IOProgram a -> StateT [FilePath] IO a
    go _ (EmptyProgram a) = return a
    go pwd (ReadFile path next) = do
      fileContents <- liftIO $ P.readFile (pwd ++ "/" ++ path)
      go pwd (next fileContents)
    go pwd (WriteFile path contents next) = do
      liftIO $ P.writeFile (pwd ++ "/" ++ path) contents
      go pwd next
    go pwd (TempFile contents next) = do
      numberOfFiles <- gets length
      let tempFileName = "temp" ++ show numberOfFiles
      let fullPath = pwd ++ "/" ++ tempFileName
      liftIO $ P.writeFile fullPath contents
      modify (fullPath :)
      go pwd (next tempFileName)
    -- Etc.

type Files = Map FilePath String
-- Look Ma, no IO!
runProgramFake :: IOProgram a -> (a, Files)
runProgramFake p = runState (go p) Map.empty where
  go :: IOProgram a -> State Files a
  go (EmptyProgram a) = return a
  go (ReadFile fp next) = do
    contents <- gets (! fp)
    go $ next contents
  go (WriteFile fp contents next) = do
    modify $ Map.insert fp contents
    go next
  go (TempFile contents next) = do
    numberOfFiles <- gets Map.size
    let tempFileName = "temp" ++ show numberOfFiles
    modify $ Map.insert tempFileName contents
    go $ next tempFileName
  -- etc.

--instance Monad IOProgram where
--  EmptyProgram a >>= f = f a
--  Composed fa >>= f = Composed $ (>>= f) <$> fa
--
--
--readFileP :: String -> IOProgram String
--readFileP fp = Composed $ ReadFile fp EmptyProgram
--writeFileP :: FilePath -> String -> IOProgram ()
--writeFileP fp contents  = Composed $ EmptyProgram <$> WriteFile fp contents ()
--tempFileP contents = Composed $ TempFile contents EmptyProgram

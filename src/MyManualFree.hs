{-# LANGUAGE DeriveFunctor #-}
module MyManualFree where

import           Control.Applicative (liftA2)
import           Control.Monad       ((>=>))
import           Control.Monad.State
import           Data.Foldable       (traverse_)
import           Data.Map.Strict     (Map, (!), (!?))
import qualified Data.Map.Strict     as Map
import           Prelude             hiding (readFile, writeFile)
import qualified Prelude             as P (readFile, writeFile)
import qualified System.Directory    as D

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
  (res, tempFiles) <- runStateT (go p) []
  traverse_ D.removeFile tempFiles
  return res where
    -- Maintain a list of all temporary files.
    go :: IOProgram a -> StateT [FilePath] IO a
    go (EmptyProgram a) = return a
    go (ReadFile path next) = do
      fileContents <- liftIO $ P.readFile (pwd ++ "/" ++ path)
      go (next fileContents)
    go (WriteFile path contents next) = do
      liftIO $ P.writeFile (pwd ++ "/" ++ path) contents
      go next
    go (TempFile contents next) = do
      numberOfFiles <- gets length
      let tempFileName = "temp" ++ show numberOfFiles
      let fullPath = pwd ++ "/" ++ tempFileName
      liftIO $ P.writeFile fullPath contents
      modify (fullPath :)
      go (next tempFileName)
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

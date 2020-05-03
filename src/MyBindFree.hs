{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE TypeOperators             #-}

module MyBindFree where

import           Control.Applicative (liftA2, (<**>))
import           Control.Monad       ((>=>))
import           Data.Functor (($>), (<$))
import Control.Monad.Writer (Writer, tell, WriterT)
import           Control.Monad.State
import           Data.Composition
import           Data.Foldable       (traverse_)
import           Data.Function       ((&))
import           Data.Map.Strict     (Map, (!), (!?))
import qualified Data.Map.Strict     as Map
import           Prelude             hiding (readFile, writeFile)
import qualified Prelude             as P (readFile, writeFile)
import qualified System.Directory    as D

-- i is called an existential type.
data Free f a = Return a | forall i. Bind (f i) (i -> Free f a)

instance Functor (Free f) where
  fmap f = (=<<) (return . f)
instance Applicative (Free f) where
  pure = Return
  fa <*> fb = fa >>= (<$> fb)
instance Monad (Free f) where
  Return a >>= f = f a
  Bind sub cont >>= f = Bind sub (cont >=> f)

infixr 0 ~>
type f ~> g = forall x. f x -> g x


interpret :: (Monad m) => (f ~> m) -> Free f a -> m a
interpret _ (Return a)      = return a
interpret f (Bind sub cont) = f sub >>= interpret f . cont

data IOProgramF a where
  ReadFile :: FilePath -> IOProgramF String
  WriteFile :: FilePath -> String -> IOProgramF ()
  TempFile :: String -> IOProgramF FilePath
  -- etc.

type IOProgram = Free IOProgramF
type Files = Map FilePath String
stateInterpreter :: IOProgramF ~> State Files
stateInterpreter (ReadFile fp) = gets (! fp)
stateInterpreter (WriteFile fp contents) =
  modify $ Map.insert fp contents
stateInterpreter (TempFile contents) = do
  numberOfFiles <- gets Map.size
  let tempFileName = "temp" ++ show numberOfFiles
  modify $ Map.insert tempFileName contents
  return tempFileName
runProgramFake :: IOProgram a -> (a, Files)
runProgramFake p = runState (interpret stateInterpreter p) Map.empty

send :: f a -> Free f a
send fa = Bind fa Return
readFile = send . ReadFile
writeFile = send .: WriteFile
tempFile = send . TempFile

anotherProgram :: IOProgram String
anotherProgram = return "hello!"
program :: IOProgram ()
program = do
  writeFile "file1" "foo"
  writeFile "file2" "bar"
  file1 <- readFile "file1"
  file2 <- readFile "file2"
  tempFile <- tempFile $ file1 ++ file2
  externalOutput <- anotherProgram
  writeFile "output" externalOutput

withPrefix :: String -> IOProgramF ~> IOProgramF
withPrefix prefix (WriteFile fp c) = WriteFile fp (prefix ++ c)
withPrefix prefix (TempFile c) = TempFile $ prefix ++ c
-- We don't care about the rest
withPrefix _ e = e

hoistFree :: (f ~> g) -> Free f a -> Free g a
hoistFree _ (Return a) = Return a
hoistFree f (Bind sub cont) = Bind (f sub) (hoistFree f <$> cont)


{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module MyJson where

import Data.Map.Strict (Map, (!?), member)
import Data.Composition
import Control.Applicative (liftA2)
import MyTree

-- TODO move to applicatives
data JsonValue = JsonString String |
  JsonInt Int |
  JsonObject (Map String JsonValue) deriving Show
type Key = String
data Error =
    InvalidType { expected:: String, actual :: String } |
    Missing | ErrorInKey Key Error deriving Show
data JsonResult a = Success a | JsonError Error deriving Show
instance Functor JsonResult where
  fmap f (Success a) = Success $ f a
  fmap _ (JsonError e) = JsonError e
instance Applicative JsonResult where
  pure = Success
  liftA2 f (Success a) (Success b) = Success $ f a b
  liftA2 _ _ (JsonError e) = JsonError e
  liftA2 _ (JsonError e) _ = JsonError e
instance Monad JsonResult where
    (Success a) >>= f = f a
    (JsonError e) >>= _ = JsonError e


newtype JsonReader a = JsonReader { runParser :: JsonValue -> JsonResult a }
instance Functor JsonReader where
  fmap f p = JsonReader $ \v -> fmap f (runParser p v)
instance Applicative JsonReader where
  pure = JsonReader . const . Success
  liftA2 f (JsonReader p1) (JsonReader p2) =
     JsonReader $ \o -> liftA2 f (p1 o) (p2 o)
instance Monad JsonReader where
  p >>= f = JsonReader $ \o -> runParser p o >>= flip runParser o . f

class JsonRead a where
  parse :: JsonReader a
invalidType expected actual =
  let actualType = case actual of
                        (JsonInt _) -> "Int"
                        (JsonString _) -> "String"
                        (JsonObject _) -> "Object"
  in JsonError $ InvalidType expected actualType
instance JsonRead Int where
  parse = JsonReader $ \case
    JsonInt x -> Success x
    e -> invalidType "Int" e
instance JsonRead String where
  parse = JsonReader $ \case
    JsonString x -> Success x
    e -> invalidType "String" e

notAnObject = invalidType "object"
parseAtKey :: JsonRead a => Key -> JsonReader a
parseAtKey key = JsonReader $ \case
  JsonObject m -> case m !? key of
    Just v -> runParser parse v
    Nothing -> JsonError $ ErrorInKey key Missing
  e -> notAnObject e
data Person = Person String Int deriving Show
instance JsonRead Person where
  parse = Person <$> parseAtKey "name" <*> parseAtKey "age"
instance JsonRead a => JsonRead (Tree a) where
  parse = JsonReader $ \case
    o@(JsonObject m) -> if "node" `member` m
                        then runParser aux o
                        else Success EmptyTree where
      aux = Tree <$> parseAtKey "node" <*>
                     parseAtKey "l" <*>
                     parseAtKey "right"
    e -> notAnObject e

data ServerError = ServerError { returnCode :: Int, errorMessage :: String }
instance JsonRead ServerError where
  parse = ServerError <$> 
          parseAtKey "returnCode" <*> parseAtKey "msg"
getPersonOrError :: JsonReader (Either Person ServerError)
getPersonOrError = do
    msgType <- parseAtKey "type"
    if msgType == "error" then Left <$> parseAtKey "error" 
                          else  Right <$> parseAtKey "data"


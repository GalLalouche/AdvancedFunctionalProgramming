{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LiberalTypeSynonyms  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module MyParsec where

import           Control.Monad.State
import Data.Map.Strict (Map, fromList)
import           Control.Applicative (liftA2)
import qualified Data.Char           as Char
import           Data.Functor        (($>))
import           Data.Text           (Text)
import qualified Data.Text           as T

type ParseResult = Either ParseError
data ParseError = ParseError {
    message :: String
  , pos     :: SourcePos
} deriving (Show, Eq, Ord)
data SourcePos = SourcePos {
    lineNumber    :: Int
  , columnNumber  :: Int
  , remainingText :: String
} deriving (Show, Eq, Ord)
isAtEnd :: SourcePos -> Bool
isAtEnd (SourcePos _ _ remainingText) = null remainingText
type Parser = StateT SourcePos ParseResult
raiseError :: String -> Parser a
raiseError e = do
  pos <- get
  lift $ Left $ ParseError e pos

mkSourcePos :: String -> SourcePos
mkSourcePos = SourcePos 0 0

parseAnyChar :: Parser Char
parseAnyChar = do
  end <- gets isAtEnd
  if end then raiseError "Parser reached end of source" else do
    h <- gets $ head . remainingText
    (modify increment) $> h where
  increment (SourcePos ln cn (h : t)) = if h == '\n' then SourcePos (ln + 1) 0 t else SourcePos ln (cn + 1) t

parseDigit :: Parser Int
parseDigit = do
  c <- parseAnyChar
  if Char.isDigit c then return $ Char.digitToInt c else raiseError $ "Expected Digit but got <" ++ [c] ++ ">"

parseNothing :: Parser ()
parseNothing = return ()

parseEither :: Parser a -> Parser b -> Parser (Either a b)
parseEither p1 p2 = StateT $ \s -> let pos = runStateT p1 s in case pos of
  Left _       -> (\(a, s) -> (Right a, s)) <$> runStateT p2 s
  Right (a, s) -> return $ (Left a, s)
-- Parses until first failure
parseMany :: Parser a -> Parser [a]
parseMany p1 = (do {x <- p1; (x :) <$> parseMany p1}) <|> return []

-- TODO NonEmpty a
parseMany1 :: Parser a -> Parser [a]
parseMany1 p = liftA2 (:) p (parseMany p)
---- Returns the first parser if successful, otherwise tries the second one
(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = StateT $ \s -> let pos = runStateT p1 s in case pos of
      Left _ -> runStateT p2 s
      Right x -> Right x

parseChar :: Char -> Parser Char
parseChar x = do
  y <- parseAnyChar
  if x == y then return y else raiseError $ "Expected " ++ show x ++ ", but got " ++ show y
parseKeyword :: String -> Parser String
parseKeyword [] = return []
parseKeyword (h : s) = liftA2 (:) (parseChar h) (parseKeyword s)

parseKeywordGood :: String -> Parser String
parseKeywordGood kw = aux [] kw where
  aux current [] = return kw
  aux current (h : s) = do
    x <- parseAnyChar
    if x /= h then raiseError $ "Expected '" ++ kw ++ "' but got '" ++ (reverse $ x : current) ++ "'"
              else aux (h : current) s

parseNot :: Char -> Parser Char
parseNot c = do
  x <- parseAnyChar
  if x == c then raiseError $ "Did not expect " ++ show c else return x
parseUntilChar :: Char -> Parser [Char]
parseUntilChar = parseMany . parseNot
parseString :: Parser String
parseString = parseChar '"' *> parseUntilChar '"' <* parseChar '"'

parseAny :: [Parser a] -> Parser a
parseAny [] = error "parseAny received an empty list! Boo on you!"
parseAny [p] = p
parseAny (h : hs) = h <|> parseAny hs

eatWhiteSpace :: Parser ()
eatWhiteSpace = void $ parseMany $ parseAny $ map parseChar " \r\n\t\v"

parseNumber :: Parser Int
parseNumber = digitsToInt 0 <$> parseMany1 parseDigit where
  digitsToInt current [] = current
  digitsToInt current (h : s) = digitsToInt (current * 10 + h) s

sepByChar :: Parser a -> Char -> Parser [a]
sepByChar p sep = (liftA2 (:) p (parseMany (parseChar sep *> p))) <|> return []

data JsValue = JsObject (Map String JsValue) | JsInt Int | JsString String | JsArray [JsValue] deriving (Show)
parseJsString = JsString <$> parseString
parseJsInt = JsInt <$> parseNumber
parseJsArray = JsArray <$> (parseChar '[' *> sepByChar (eatWhiteSpace *> parseJsValue <* eatWhiteSpace) ',' <* parseChar ']')
parseJsObject = JsObject . fromList <$> (parseChar '{' *> eatWhiteSpace *> sepByChar keyValuePair ',' <* eatWhiteSpace <* parseChar '}') where
  keyValuePair :: Parser (String, JsValue)
  keyValuePair = liftA2 (,) (eatWhiteSpace *> parseString) (eatWhiteSpace *> parseChar ':' *> eatWhiteSpace *> parseJsValue <* eatWhiteSpace)
parseJsValue = parseAny [parseJsString, parseJsInt, parseJsObject, parseJsArray]

{-# LANGUAGE TupleSections #-}

module MyParsec where

import           Control.Applicative (liftA2)
import           Control.Monad.State
import qualified Data.Char           as Char
import           Data.Functor        (($>))
import           Data.List.NonEmpty  (NonEmpty((:|)))
import qualified Data.List.NonEmpty  as NE
import           Data.Map.Strict     (Map, fromList)
import           Data.Text           (Text)
import qualified Data.Text           as T

type ParseError = (String, SourcePos)
type ParseResult = Either ParseError
data SourcePos = SourcePos {
    lineNumber    :: Int
  , columnNumber  :: Int
  , remainingText :: String
} deriving (Show, Eq, Ord)
mkSourcePos :: String -> SourcePos
mkSourcePos = SourcePos 0 0

type Parser = StateT SourcePos ParseResult

raiseError :: String -> Parser a
raiseError s = do { pos <- get; lift $ Left $ (s, pos) }

anyChar :: Parser Char
anyChar = do
  isAtEnd <- gets $ null . remainingText
  if isAtEnd
     then raiseError "Parser reached end of source"
     else gets (head . remainingText) <* modify increment
  where
    increment (SourcePos ln cn (h : t)) =
      if h == '\n' then SourcePos (ln + 1) 0 t
                   else SourcePos ln (cn + 1) t

digit :: Parser Int
digit = do
  c <- anyChar
  if Char.isDigit c then return $ Char.digitToInt c else raiseError $ "Expected Digit but got <" ++ [c] ++ ">"

parseTwoDigits = do
  x <- digit
  y <- digit
  return $ [x, y]

digitsToInteger :: Foldable f => f Int -> Integer
digitsToInteger = foldl (\a b -> (a * 10) + (toInteger b)) 0
parseNumberSlow :: Parser Integer
parseNumberSlow =
    digitsToInteger <$> liftA2 (:) digit parseDigits where
  parseDigits :: Parser [Int]
  parseDigits = (liftA2 (:) digit parseDigits) <|> pure []


-- Runs parser p2 if p1 failed *without consuming any input*
(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = StateT $ \s1 -> case runStateT p1 s1 of
  l@(Left (_, s2)) -> if areSame s1 s2 then runStateT p2 s1 else l
  x                -> x
  where
    areSame (SourcePos ln1 cn1 _) (SourcePos ln2 cn2 _) =
      ln1 == ln2 && cn1 == cn2
--
--parseNothing :: Parser ()
--parseNothing = return ()



--parseEither :: Parser a -> Parser b -> Parser (Either a b)
--parseEither p1 p2 = StateT $ \s -> let pos = runStateT p1 s in case pos of
--  Left _       -> (\(a, s) -> (Right a, s)) <$> runStateT p2 s
--  Right (a, s) -> return $ (Left a, s)
-- Parses until first failure
try :: Parser a -> Parser a
try p = StateT $ \s1 -> case runStateT p s1 of
  Left (x, _) -> Left (x, s1) -- Return original state
  x           -> x

--StateT $ \s -> let pos = runStateT p s in case pos of
--      Left _  -> runStateT p2 s
--      Right x -> Right x
many :: Parser a -> Parser [a]
many p = try (liftA2 (:) p (many p)) <|> pure []

many1 :: Parser a -> Parser (NonEmpty a)
many1 p = liftA2 (:|) p (many p)
---- Returns the first parser if successful, otherwise tries the second one
--(<|>) :: Parser a -> Parser a -> Parser a
--p1 <|> p2 = StateT $ \s -> let pos = runStateT p1 s in case pos of
--      Left _  -> runStateT p2 s
--      Right x -> Right x

char :: Char -> Parser Char
char x = do
  y <- anyChar
  if x == y then return y else raiseError $ "Expected " ++ show x ++ ", but got " ++ show y
keyword :: String -> Parser String
keyword []      = return []
keyword (h : s) = liftA2 (:) (char h) (keyword s)

parseKeywordGood :: String -> Parser String
parseKeywordGood kw = aux [] kw where
  aux current [] = return kw
  aux current (h : s) = do
    x <- anyChar
    if x /= h then raiseError $ "Expected '" ++ kw ++ "' but got '" ++ (reverse $ x : current) ++ "'"
              else aux (h : current) s

parseNot :: Char -> Parser Char
parseNot c = do
  x <- anyChar
  if x == c then raiseError $ "Did not expect " ++ show c else return x
parseUntilChar :: Char -> Parser [Char]
parseUntilChar = many . parseNot
parseString :: Parser String
parseString = char '"' *> parseUntilChar '"' <* char '"'

parseAny :: [Parser a] -> Parser a
parseAny []       = error "parseAny received an empty list! Boo on you!"
parseAny [p]      = p
parseAny (h : hs) = h <|> parseAny hs

eatWhiteSpace :: Parser ()
eatWhiteSpace = void $ many $ parseAny $ map char " \r\n\t\v"

number :: Parser Integer
number = digitsToInteger <$> many1 digit

sepByChar :: Parser a -> Char -> Parser [a]
sepByChar p sep = (liftA2 (:) p (many (char sep *> p))) <|> return []

data JsValue = JsObject (Map String JsValue) | JsInt Integer | JsString String | JsArray [JsValue] deriving (Show)
parseJsString = JsString <$> parseString
parseJsInt = JsInt <$> number
parseJsArray = JsArray <$> (char '[' *> sepByChar (eatWhiteSpace *> parseJsValue <* eatWhiteSpace) ',' <* char ']')
parseJsObject = JsObject . fromList <$> (char '{' *> eatWhiteSpace *> sepByChar keyValuePair ',' <* eatWhiteSpace <* char '}') where
  keyValuePair :: Parser (String, JsValue)
  keyValuePair = liftA2 (,) (eatWhiteSpace *> parseString) (eatWhiteSpace *> char ':' *> eatWhiteSpace *> parseJsValue <* eatWhiteSpace)
parseJsValue = parseAny [parseJsString, parseJsInt, parseJsObject, parseJsArray]

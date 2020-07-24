{-# LANGUAGE RecordWildCards #-}
module Main where

--import MyEq
import DS.ResizableVector
--import MyShow
import MyMeta
--import Prelude hiding (Monad(..), Applicative(..), Eq (..), Show(..))
import Control.Monad((>>))
import Prelude hiding (lookup)

data Point = Point {x :: Int, y :: Int}
getPoint = let x = 3; y = 4 in Point {..}

foo = let
  Point {..} = getPoint
  in x + y
--instance Foldable Tree where
--  foldMap f EmptyTree = mempty
--  foldMap f (Tree a left right) = foldMap f left `mappend` f a `mappend` foldMap f right
--
--data DateTime = DateTime Int deriving Show
--getCurrentTime :: IO DateTime
--getCurrentTime = undefined
--
--getValueFromServer :: IO String
--getValueFromServer = undefined
--foo = getCurrentTime >>= (\time ->
--  getValueFromServer >>= \value ->
--    let str = show time ++ " " ++ value
--    in writeFile "log.txt" str *> print str)
--withDoSyntax = do
--  time <- getCurrentTime -- Monadic values use <-
--  value <- getValueFromServer
--  let str = show time ++ " " ++ value -- Pure values use let
--  -- The last return value has to be a monad
--  writeFile "log.txt" str *> print str
--excited :: String -> String
--excited = flip (++) "!"
--runParser parser source = print $ runStateT parser (mkSourcePos source)

data C = C {a :: Int, b :: Int}
data D = D {c :: Int, d :: Int}
data E = E {a :: Int, b :: Int, c :: Int, d :: Int}
getC :: C
getC = undefined
getD :: D
getD = undefined
cdToE :: C -> D -> E
cdToE (C {..}), (D {..}) = E {..}

data Comment = Comment {
      id           :: Int
    , content      :: Int
    , reviewId     :: Int
    , submissionId :: Int
    , conferenceId :: Int
    , date         :: Int
    , reviewerNumber :: Int
    -- Etc.
  }
aux :: [ResizableVector Int -> ResizableVector Int] -> IO ()
aux = go empty where
  go _ [] = return ()
  go e (f : fs) = do
    let next = f e
    print next
    go next fs


main = do
--  aux $ map pushr [0..20]
  let v = fromList [0..7]
  print v
  print $ lookup 2 $ fromList [0..3]
  print $ lookup 6 $ fromList [0..7]
  print $ lookup 14 $ fromList [0..20]
  print $ update 14 42 $ fromList [0..20]
--  print $ (Prod 1 [1, 2]) == (Prod 1 [1, 2])
--  print $ (Prod 1 [1, 2]) == (Prod 1 [1, 2])
--  runParser (char 'f') "foobar"
----  runParser (string "abc") "ab123"
----  go (many parseTwoDigits) "121a"
----  go digit "123"
----  go parseNumberSlow "123"
------  go (parseEither parseDigit parseAnyChar) "123"
------  go (parseEither parseDigit parseAnyChar) "abc"
----  go (many digit) "123abc"
----  go (keyword "abc") "abc123"
----  go (keyword "abc") "abc123"
----  go (parseKeywordGood "123") "abc123"
----  go parseString "\"hello world!\""
----  go number "12324"
----  go (sepByChar number ',') ""
----  go (sepByChar number ',') "123,234,345"
--  runParser (sepByComma number) "1, 2, 3"
--  runParser (sepByComma number) ""
----  runParser parseJsValue "{\"foo\": 123, \"bar\": \"bazz\", \"obj\": {\"quxx\": 234}}"
--  runParser parseJsValue "{}"
--  runParser parseJsValue "[]"
--  runParser parseJsValue "{\"foo\": 123, \"bar\": \"bazz\", \"obj\": {\"quxx\": 234, \"arr\": [456, \"str\", []]}}"
--  runParser parseJsValue "{\"foo\": 123, \"bar\": \"bazz\", \"obj\": {\"quxx\": 234, \"arr\": [456, \"str\"]}}"
--  runParser parseJsValue "[12,3, 4 ]"
--  print $ property_approx1
--  print $ property_approx2
--  print $ property_checkGoodGcd
--  print $ property_checkBadGcd
--  print $ property_checkReverseGood
--  print $ property_checkReverseBad
----  print $ tree
-- -  print $ take 5 $ toList tree

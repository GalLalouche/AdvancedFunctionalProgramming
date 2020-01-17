{-# LANGUAGE TupleSections #-}
module MyState where

import Control.Applicative

-- Given an existing state, runs the computation,
-- returning a value and a new state.
newtype State s a = State { runState :: s -> (a, s) }
instance Functor (State s) where
  fmap f (State runState) = State $ \s -> let (a, newState) = runState s in (f a, newState)
instance Applicative (State s) where
  pure x = State (x,)
  liftA2 f (State r1) (State r2) = State $ \s -> let
      (a1, s1) = r1 s
      (a2, s2) = r2 s1
    in (f a1 a2, s2)
instance Monad (State s) where
  (State runState) >>= f = State $ \s -> let 
      (a, newState) = runState s 
      (State newRunState) = f a
    in newRunState newState

get :: State s s
get = State $ \s -> (s, s)
gets :: (s -> a) -> State s a
gets f = State $ \s -> (f s, s)
put :: s -> State s ()
put s = State $ const ((), s)
modify :: (s -> s) -> State s ()
modify f = do
  oldState <- get
  put $ f oldState
  return ()

type ItemName = String
type Price = Int
data ShoppingList = ShoppingList {
    getPrice :: Price,
    getItems :: [ItemName] } deriving Show

addItem :: ItemName -> Price -> State ShoppingList ()
addItem name price = do
  currentPrice <- gets getPrice
  currentItems <- gets getItems
  put $ ShoppingList 
      (price + currentPrice) (name : currentItems)
  return ()

thirdItemFree :: ItemName -> Price -> State ShoppingList ()
thirdItemFree name price = do
  addItem name price
  (ShoppingList currentPrice currentItems) <- get
  let updatedPrice = if length currentItems == 3 
      then currentPrice - price else currentPrice
  put $ ShoppingList updatedPrice currentItems
  return ()


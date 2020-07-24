{-# LANGUAGE MultiParamTypeClasses #-}
module MyZipper where

class Zippable t d where
  go :: d -> t a -> t a


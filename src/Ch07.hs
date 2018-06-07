module Ch07
  ( twice
  ) where

twice :: (a -> a) -> a -> a
twice f x = f (f x)
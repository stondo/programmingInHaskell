module Ch06
  ( fac
  , insert
  , isort
  , fib
  , evens
  , odds
  , fac'
  ) where

fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n - 1)

insert :: Ord a => a -> [a] -> [a]
insert x []                 = [x]
insert x (y:ys) | x <= y    = x : y : ys
               | otherwise  = y : insert x ys

isort :: Ord a => [a] -> [a]
isort []     = []
isort (x:xs) = insert x (isort xs)

-- 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, ...
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

evens :: [a] -> [a]
evens []     = []
evens (x:xs) = x : odds xs

odds :: [a] -> [a]
odds []     = []
odds (_:xs) = evens xs

-- Exercises

-- 1.
fac' :: Int -> Int
fac' 0 = 1
fac' n | n >= 0 = n * fac (n - 1)
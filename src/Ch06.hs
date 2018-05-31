module Ch06
  ( fac
  , insert
  , isort
  , fib
  , evens
  , odds
  , fac'
  , sumdown
  , powNonNegative
  , euclid
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

-- 2.
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)

-- 3.
powNonNegative :: Int -> Int -> Int
powNonNegative 0 n | n >= 0         = 0
powNonNegative n 0 | n > 0          = 1
powNonNegative n m | n > 0 && m >= 0 = n * (^) n (m - 1)

-- 4.
euclid :: Int -> Int -> Int
euclid n m | n == m    = n
euclid n m | n > m     = euclid m (n - m)
euclid n m | n < m     = euclid n (m - n)

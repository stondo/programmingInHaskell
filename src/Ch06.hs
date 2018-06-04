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
  , and'
  , concat'
  , replicate'
  , selNth
  , elem'
  , merge
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
powNonNegative n m | n > 0 && m >= 0 = n * powNonNegative n (m - 1)

-- 4.
euclid :: Int -> Int -> Int
euclid n m | n == m    = n
euclid n m | n > m     = euclid m (n - m)
           | otherwise = euclid n (m - n)
--euclid n m | n < m     = euclid n (m - n)

-- 6.
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) | x = and xs
            | otherwise = False
concat' :: [[a]] -> [a]
concat' (x:xs) | not (null xs) = x ++ concat' xs
               | otherwise = x

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n a | n > 0 = a : replicate' (n - 1) a

selNth :: [a] -> Int -> a
selNth xs n | n == 0 = head xs
            | n > 0  = selNth t (n - 1)
  where t = tail xs

elem' :: Eq a => a -> [a] -> Bool
elem' e [] = False
elem' e (x:xs) | x == e = True
               | otherwise = elem' e xs

-- 7.
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x <= y && not (null ys) && y <= minimum ys                 = x : y : merge xs ys
                    | x <= y && not (null ys) && y >  minimum ys                 = merge (x:xs) (ys ++ [y])
                    | x > y                  = merge (y:xs) (x:ys)
                    -- | x > y && not (null ys) && x >= (maximum ys) = y : merge xs ys ++ [x]
                    -- | x > y && not (null ys) && x < (maximum ys) = merge (y:xs) (x:ys)
                    | otherwise              = y : x : merge xs ys



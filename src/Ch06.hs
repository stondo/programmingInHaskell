module Ch06
  ( fac
  , insert
  , isort
  , fib
  , evensPos
  , oddsPos
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
  , halve
  , msort
  , sumList
  , takeRec
  , lastRec
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

evensPos :: [a] -> [a]
evensPos []     = []
evensPos (x:xs) = x : oddsPos xs

oddsPos :: [a] -> [a]
oddsPos []     = []
oddsPos (_:xs) = evensPos xs

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
merge (x:xs) (y:ys) | x <= y    = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys
-- 8.
halve :: [a] -> ([a],[a])
halve xs = (take ((length xs) `div` 2) xs, drop ((length xs) `div` 2) xs)

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = merge (isort a) (isort b)
  where (a,b) = halve xs

-- 9.

-- a.
sumList :: Num a => [a] -> a
sumList []     = 0
sumList [x]    = x
sumList (x:xs) = x + sumList xs

-- b.
takeRec :: Int -> [a] -> [a]
takeRec _ []     = []
takeRec n (x:xs) | n > 0 && n >= length (x:xs) = x:xs
                 | n > 0 && n < length (x:xs) = x : takeRec (n - 1) xs
                 | otherwise                   = []

lastRec :: [a] -> a
lastRec (x:xs) | null xs   = x
               | otherwise = last xs
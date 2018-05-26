module Ch05
  ( concat'
  , firsts
  , length'
  , factors
  , prime
  , primes
  , find
  , pairs
  , sorted
  , positions
  , lowers
  , count
  , let2int
  , int2let
  , shift
  , encode
  , table
  , percent
  , freqs
  , freqsChar
  , chisqr
  , rotate
  , crack
  , sumSqrd
  , grid
  , square
  , replicate'
  , pyths
  , perfects
  , positions'
  , scalarproduct
  ) where

import Data.Char

concat' :: [[a]] -> [a]
concat' xss = [x | xs <- xss, x <- xs]

firsts :: [(a,b)] -> [a]
firsts xs = [x | (x, _) <- xs]

length' :: [a] -> Int
length' xs = sum [1 | _ <- xs]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1, n] -- || factors n == [1]

primes :: Int -> [Int]
primes n = [x | x <- [1..n], prime x]

find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

pairs :: [a] -> [(a, a)]
pairs xs = xs `zip` tail xs

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x,y) <- pairs xs]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (c,i) <- xs `zip` [0..], c == x]

lowers :: String -> Int
lowers xs = length' [x | x <- xs, x >= 'a' && x <= 'z']

count :: Char -> String -> Int
count x xs = length' [x | x' <- xs, x == x']

-- The Cesar cipher

--let2int :: Char -> Int
--let2int c = ord c - ord 'a'
let2int :: Char -> Int
let2int c = if (isLower c) then ord c - ord 'a' else ord c - ord 'A'

--int2let :: Int -> Char
--int2let n = chr (ord 'a' + n)
int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

--shift :: Int -> Char -> Char
--shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
--          | otherwise = c
shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n c | c <- xs]

table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,
         0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,
         6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

--freqs :: String -> [Float]
--freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
--           where n = lowers xs
freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z'] ++ ['A'..'Z']]
           where n = length xs

freqsChar :: String -> [(Char, Float)]
freqsChar xs = [(x, percent (count x xs) n) | x <- xs]
           where n = lowers xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2)/e | (o,e) <- os `zip` es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack :: String -> String
crack xs = encode (-factor) xs
  where factor = head (positions (minimum chitab) chitab)
        chitab = [chisqr (rotate n table') table | n <- [0..25]]
        table' = freqs xs

-- Exercises

-- 1.
sumSqrd :: Int -> Int
sumSqrd n = sum [x^2 | x <- [1..n]]

-- 2.
grid :: Int -> Int -> [(Int, Int)]
grid n m = [(x, y) | x <- [0..n], y <- [0..m]]

-- 3.
square :: Int -> [(Int, Int)]
square n = [(x,y) | (x,y) <- grid n n, x /= y]

-- 4.
replicate' :: Int -> a -> [a]
replicate' n t = [t | _ <- [1..n]]

-- 5.
pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

-- 6.
perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (init (factors x)) == x]

-- 7.
-- [(x,y') | x <- [1,2], y' <- [y | y <- [3,4]]] == concat[ [(x,y) | y <- [3,4]] | x <- [1,2]]

-- 8.
positions' :: Eq a => a -> [a] -> [Int]
positions' x xs = find x (xs `zip` [0..])

-- 9.
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x,y) <- xs `zip` ys]


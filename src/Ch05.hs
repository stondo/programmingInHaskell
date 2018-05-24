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

positions :: Eq a => a -> [a] -> [Integer]
positions x xs = [i | (c,i) <- xs `zip` [0..], c == x]

lowers :: String -> Int
lowers xs = length' [x | x <- xs, x >= 'a' && x <= 'z']

count :: Char -> String -> Int
count x xs = length' [x | x' <- xs, x == x']

-- The Cesar cipher

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n c | c <- xs]
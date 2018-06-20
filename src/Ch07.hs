module Ch07
  ( twice
  , sumsqreven
  , sumsqreven'
  , sumsqreven''
  , bin2int
  , bin2int'
  , int2bin
  , make8
  , encode
  , chop8
  , decode
  , transmit
  , channel
  , votes
  , count
  , rmdups
  , result
  , winner
  , ballots
  , rmempty
  , elim
  , rank
  , winner'
  , all'
  , any'
  , takeWhile'
  , takeWhile''
  , dropWhile'
  , map'
  , filter'
  , dec2int
  , curry'
  , uncurry'
  , unfold
  , int2binUnfold
  , chop8Unfold
  , mapUnfold
  , iterateUnfold
  ) where

import Data.Char
import Data.List

import Debug.Trace

debug = flip trace

twice :: (a -> a) -> a -> a
twice f x = f (f x)

sumsqreven :: [Int] -> Int
sumsqreven [] = 0
sumsqreven (x:xs) | even x    = x^2 + sumsqreven xs
                  | otherwise = sumsqreven xs

sumsqreven' :: [Int] -> Int
sumsqreven' xs = sum (map (^2) (filter even xs))

sumsqreven'' :: [Int] -> Int
sumsqreven'' = sum . map (^2) . filter even


-- Binary String Transmitter
type Bit = Int

bin2int' :: [Bit] -> Int
bin2int' bits = sum [w*b | (w,b) <- zip weights bits]
               where weights = iterate (*2) 1

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

-- Voting Algorithms

-- First-past-the-post
votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

rmdups :: Eq a => [a] -> [a]
rmdups []     = []
rmdups (x:xs) = x : rmdups (filter (/=x) xs)
-- rmdups (x:xs) = x : filter (/= x) (rmdups xs)



result :: Ord a => [a] -> [(Int,a)]
result vs = sort [(count v vs, v) | v <- rmdups vs]

winner :: Ord a => [a] -> a
winner = snd . last . result

-- Alternative vote
ballots :: [[String]]
ballots = [
           ["Red", "Green"],
           ["Blue"],
           ["Green", "Red", "Blue"],
           ["Blue", "Gren", "Red"],
           ["Green"]
          ]

rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

winner' :: Ord a => [[a]] -> a
winner' bs = case rank (rmempty bs) of
                [c]    -> c
                (c:cs) -> winner' (elim c bs)

-- Exercises

-- 1.
-- [f x | x <- xs, p x] == map (f) filter (p) xs

-- 2.

-- a.
all' :: (a -> Bool) -> [a] -> Bool
all' f = and . map f

-- b.
any' :: (a -> Bool) -> [a] -> Bool
any' f = or . map f

-- c.
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f = foldr loop []
                 where loop x xs
                                 | f x       = x:xs
                                 | otherwise = []
--takeWhile' f = foldr (\(x:xs) -> if (f x) then x else xs) []

takeWhile'' :: (a -> Bool) -> [a] -> [a]
takeWhile'' _ []                 = []
takeWhile'' f (x:xs) | f x       = x : takeWhile'' f xs
                     | otherwise = []


dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ []     = []
dropWhile' f (x:xs) | f x       = dropWhile' f xs
                    | otherwise = x:xs

-- 3.
-- map :: (a -> b) -> [a] -> [b]
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr loop []
              where loop e acc | p e       =  e:acc
                               | otherwise =  acc

-- 4.
dec2int :: [Int] -> Int
dec2int = foldl ((+) . (*10)) 0
-- dec2int = scanl ((+) . (*10)) 0
-- foldl (\acc x -> traceShow acc (x + acc * 10)) 0 [2,3,4,5]
-- foldl (\acc x -> traceShow (acc * 10, x) (x + acc * 10)) 0 [2,3,4,5]

-- 5. https://www.reddit.com/r/haskell/comments/uukdp/definitions_of_curry_and_uncurry/
curry' :: ((a, b) -> c) -> (a -> b -> c)
curry' f = \x y -> f (x, y)

uncurry' :: ((a -> b -> c) ->  (a, b) -> c)
uncurry' f = \(x, y) -> f x y

-- myAdd (a,b) = a + b
-- myAddCurried = curry' myAdd
-- myAddUncurried = uncurry' myAddCurried

-- myAdd :: Num a => (a, a) -> a
-- myAddCurried :: Num c => c -> c -> c
-- myAddUncurried :: Num c => (c, c) -> c

-- 6.
unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)

int2binUnfold = unfold (== 0) (`mod` 2) (`div` 2)

chop8Unfold = unfold (null) (take 8) (drop 8) 

mapUnfold f = unfold (null) (f . head) (tail)

iterateUnfold f = unfold (not . null) (f) (id)

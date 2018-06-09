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
  ) where

import Data.Char
import Data.List

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
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

result :: Ord a => [a] -> [(Int,a)]
result vs = sort [(count v vs, v) | v <- rmdups vs]

winner :: Ord a => [a] -> a
winner = snd . last . result

-- Alternative vote


-- Exercises




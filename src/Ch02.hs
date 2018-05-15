module Ch02 where

double :: Int -> Int
double x = x + x

quadruple :: Int -> Int
quadruple x = double (double x)

factorial :: Int -> Int
factorial n = product [1..n]

average :: Foldable t => t Int -> Int
average ns = sum ns `div` length ns

-- Exrcises

-- 4.
--last [1,2,3,4,5] == head (reverse [1,2,3,4,5])
last' :: [a] -> a
last' ns = head(drop(length ns - 1) ns)

-- 5.
--init [1,2,3,4,5] == reverse (drop 1 (reverse [1,2,3,4,5]))
--init [1,2,3,4,5] == take (length [1,2,3,4,5] - 1) [1,2,3,4,5]
init' :: [a] -> [a]
init' ns = take(length ns - 1) ns

init'' :: [a] -> [a]
init'' ns = reverse(drop 1 (reverse ns))

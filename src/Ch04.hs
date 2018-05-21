module Ch04
  ( even'
  , splitAt'
  , recip'
  , abs'
  , signum'
  , fst'
  , snd'
  , abs''
  , signum''
  , addLambda
  , odds
  , odds'
  , sumFoldl
  , halve
  , third
  , third'
  , third''
  , safetail
  , safetail'
  , safetail''
  , multLambda
  , luhnDouble
  , luhnDouble'
  , luhn
  ) where

even' :: Integral a => a -> Bool
even' n = n `mod` 2 == 0

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' n ns = (take n ns, drop n ns)

recip' :: Fractional a => a -> a
recip' n = 1/n

abs' :: Int -> Int
abs' x = if x >= 0 then x else -x

signum' :: Int -> Int
signum' n = if n < 0 then -1 else
              if n == 0 then 0 else 1

fst' :: (a, b) -> a
fst' (a, _) = a

snd' :: (a, b) -> b
snd' (_, b) = b

abs'' :: Int -> Int
abs'' n | n >= 0    =  n
       | otherwise = -n

signum'' :: Int -> Int
signum'' n | n < 0     = -1
          | n == 0    = 0
          | otherwise = 1

addLambda :: Int -> (Int -> Int)
addLambda = \x -> (\y -> x + y)

odds :: Int -> [Int]
odds n = map f [0..n-1]
         where f x = x * 2 + 1

odds' :: Int -> [Int]
odds' n = map (\x -> x * 2 + 1) [0..n-1]

sumFoldl :: [Int] -> Int
sumFoldl = foldl (+) 0

-- Exercises

-- 1.
halve :: [a] -> ([a], [a])
halve xs = (take(length xs `div` 2) xs, drop(length xs `div` 2) xs)

-- 2.
third :: [a] -> a
third xs = xs !! (3 - 1)

third' :: [a] -> a
third' xs = head (tail (tail xs))

third'' :: [Int] -> Int
third'' (_ : _ : x : _)     = x
third'' _          = -1

-- 3.
safetail :: [a] -> [a]
safetail (x : xs) = if null xs then [] else xs
safetail _ = []

safetail' :: [a] -> [a]
safetail' xs = if length xs == 1 || length xs == 0 then [] else tail xs

safetail'' :: [a] -> [a]
safetail'' xs | null xs    = []
              | otherwise  = tail xs

-- 7.
multLambda :: Int -> (Int -> (Int -> Int))
multLambda = \x -> (\y -> (\z -> x * y * z))

--mult' :: Int -> Int -> Int -> Int
--mult' x y z = x * y * z

-- 8.
luhnDouble :: Int -> Int
luhnDouble n | n * 2 <= 9 = n * 2
             | otherwise  = n * 2 - 9

luhnDouble' :: Int -> Int
luhnDouble' n = if (n * 2) > 9 then n * 2 - 9 else n * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = (d + luhnDouble c + b + luhnDouble a) `mod` 10 == 0

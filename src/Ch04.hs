module Ch04 where

even :: Integral a => a -> Bool
even n = n `mod` 2 == 0

splitAt :: Int -> [a] -> ([a], [a])
splitAt n ns = (take n ns, drop n ns)

recip :: Fractional a => a -> a
recip n = 1/n

abs :: Int -> Int
abs x = if x >= 0 then x else -x

signum :: Int -> Int
signum n = if n < 0 then -1 else
              if n == 0 then 0 else 1

abs' :: Int -> Int
abs' n | n >= 0    =  n
       | otherwise = -n

signum' :: Int -> Int
signum' n | n < 0     = -1
          | n == 0    = 0
          | otherwise = 1

add :: Int -> (Int -> Int)
add = \x -> (\y -> x + y)

odds :: Int -> [Int]
odds n = map f [0..n-1]
         where f x = x * 2 + 1

odds' :: Int -> [Int]
odds' n = map (\x -> x * 2 + 1) [0..n-1]

sum :: [Int] -> Int
sum = foldl (+) 0

-- Exercises

-- 1.
halve :: [a] -> ([a], [a])
halve xs = (take((length xs) `div` 2)(xs), drop((length xs) `div` 2)(xs))

third :: [a] -> a
third xs = xs !! (3 - 1)

third' :: [a] -> a
third' xs = head (tail (tail xs))

third'' :: [Int] -> Int
third'' (_ : _ : x : _)     = x
third'' _          = -1

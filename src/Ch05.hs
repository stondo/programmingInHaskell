module Ch05 where

concat :: [[a]] -> [a]
concat xss = [x | xs <- xss, x <- xs]

firsts :: [(a,b)] -> [a]
firsts xs = [x | (x, _) <- xs]

length' :: [a] -> Int
length' xs = sum [1 | _ <- xs]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]
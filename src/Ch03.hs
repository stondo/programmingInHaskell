module Ch03 where

-- Functions
add :: (Int, Int) -> Int
add (x,y) = x + y

zeroto :: Int -> [Int]
zeroto n = [0..n]

--Curried Functions
add' :: Int -> (Int -> Int)
add' x y = x + y

--mult :: Int -> (Int -> (Int -> Int))
mult :: Int -> Int -> Int -> Int
mult x y z = x * y * z -- ((mult x) y ) z
module Ch07
  ( twice
  , sumsqreven
  , sumsqreven'
  , sumsqreven''
  ) where

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
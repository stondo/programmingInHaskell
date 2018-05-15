module Ch01 where

double :: Int -> Int
double x = x + x

recSum :: Num a => [a] -> a
recSum [] = 0
recSum (n:ns) = n + sum ns

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                   smaller = [a | a <- xs, a <= x]
                   larger = [b | b <- xs, b > x]

--seqn :: [IO a] -> IO [a]
seqn :: Monad m => [m a] -> m [a]
seqn [] = return []
seqn (act:acts) = do x <- act
                     xs <- seqn acts
                     return (x:xs)

-- Exercises

-- 3.
prod :: Num a => [a] -> a
prod [] = 1
prod (n:ns) = n * prod ns

-- 4.
qsortReversed :: Ord a => [a] -> [a]
qsortReversed [] = []
qsortReversed (x:xs) = qsortReversed larger ++ [x] ++ qsortReversed smaller
               where
                   smaller = [a | a <- xs, a < x]
                   larger = [b | b <- xs, b >= x]

-- 5.
qsortWithoutDuplicates :: Ord a => [a] -> [a]
qsortWithoutDuplicates [] = []
qsortWithoutDuplicates (x:xs) = qsortWithoutDuplicates smaller ++ [x] ++ qsortWithoutDuplicates larger
               where
                   smaller = [a | a <- xs, a < x]
                   larger = [b | b <- xs, b > x]

qsortWithoutDuplicatesReversed :: Ord a => [a] -> [a]
qsortWithoutDuplicatesReversed [] = []
qsortWithoutDuplicatesReversed (x:xs) = qsortWithoutDuplicatesReversed larger ++ [x] ++ qsortWithoutDuplicatesReversed smaller
               where
                   smaller = [a | a <- xs, a < x]
                   larger = [b | b <- xs, b > x]
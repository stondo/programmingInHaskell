module Lib
    ( someFunc
    ) where

import Ch09 (solutions, solutions', solutions'')

someFunc :: IO ()
someFunc = print (solutions'' [1,3,7,10,25,50] 765)
-- someFunc = print (solutions' [1,3,7,10,25,50] 765)
-- someFunc = print (solutions [1,3,7,10,25,50] 765)
-- someFunc = putStrLn "someFunc"

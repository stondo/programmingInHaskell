module Lib
    ( someFunc
    ) where

import Ch09 (solutions, solutions', solutions'', countExprs)

someFunc :: IO ()
-- someFunc = print (countExprs [1,3,7,10,25,50])
someFunc = print (solutions'' [1,3,7,10,25,50] 765)
-- someFunc = print (length (solutions'' [1,3,7,10,25,50] 765))
-- someFunc = print (solutions'' [1..7] 1979)
-- someFunc = print (solutions' [1,3,7,10,25,50] 765)
-- someFunc = print (solutions [1,3,7,10,25,50] 765)
-- someFunc = putStrLn "someFunc"

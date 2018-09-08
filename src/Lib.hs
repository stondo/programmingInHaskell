module Lib
    ( someFunc
    ) where

import Ch09 (solutions, solutions', solutions'', countExprs, solutionsOrClosest)
import Ch11 (playTicTacToe, empty, Player (..))
import System.IO

someFunc :: IO ()
someFunc = do hSetBuffering stdout NoBuffering
              playTicTacToe empty O ""
-- someFunc = print (countExprs [1,3,7,10,25,50])
-- someFunc = print (solutionsOrClosest [1,3,7,10,25,50] 831 1)
-- someFunc = print (length (solutionsOrClosest [1,3,7,10,25,50] 831) 1)
-- someFunc = print (solutions'' [1,3,7,10,25,50] 831)
-- someFunc = print (solutions'' [1,3,7,10,25,50] 765)
-- someFunc = print (length (solutions [1,3,7,10,25,50] 765))
-- someFunc = print (length (solutions'' [1,3,7,10,25,50] 765))
-- someFunc = print (solutions'' [1..7] 1979)
-- someFunc = print (solutions' [1,3,7,10,25,50] 765)
-- someFunc = print (solutions [1,3,7,10,25,50] 765)
-- someFunc = putStrLn "someFunc"

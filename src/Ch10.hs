module Ch10
  ( act
  , getLine'
  , putStr'
  , putStrLn'
  , strLen
  , hangman
  , next
  , initial
  , finished
  , valid
  , move
  , putRow
  , putBoard
  , getDigit
  , newline
  , playNim
  , nim
  , cls
  , Pos
  , writeAt
  , goto
  , width
  , height
  , GoLBoard
  , glider
  , showCells
  , isEmpty
  , neighbs
  , wrap
  , liveNeighbs
  , survivors
  , births
  , nextGen
  , life
  , wait
  , expOne
  ) where

import System.IO (hSetEcho, stdin)
import Data.Char (digitToInt, isDigit)
import Ch07 (rmdups)


act :: IO (Char,Char)
act = do x <- getChar
         getChar
         y <- getChar
         return (x,y)

getLine' :: IO String
getLine' = do x <- getChar
              if x == '\n' then
                 return []
              else
                do xs <- getLine'
                   return (x:xs)

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = do putChar x
                    putStr' xs

putStrLn' :: String -> IO ()
putStrLn' xs = do putStr' xs
                  putChar '\n'

strLen :: IO ()
strLen = do putStr' "Enter a string: "
            xs <- getLine'
            putStr' "The string has: "
            putStr' (show (length xs))
            putStrLn' " characters"


-- 10.6 Hangman
hangman :: IO ()
hangman = do putStrLn' "Think of a word:"
             word <- sgetLine
             putStrLn' "Try to guess it:"
             play word

sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
                 do putChar x
                    return []
              else
                 do putChar '-'
                    xs <- sgetLine
                    return (x:xs)

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x


play :: String -> IO ()
play word = do putStr' "? "
               guess <- getLine'
               if guess == word then
                  putStrLn' "You got it!!"
               else
                  do putStrLn (match word guess)
                     play word

match :: String -> String -> String
match xs ys = [if elem x ys then x else '-' | x <- xs]

-- 10.7 Nim
next :: Int -> Int
next 1 = 2
next 2 = 1

type NimBoard = [Int]

initial :: NimBoard
initial = [5,4,3,2,1]

finished :: NimBoard -> Bool
finished = all (== 0)

valid :: NimBoard -> Int -> Int -> Bool
valid board row num = board !! (row - 1) >= num

move :: NimBoard -> Int -> Int -> NimBoard
move board row num = [update r n | (r,n) <- zip [1..] board]
  where update r n = if r == row then n - num else n

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

putBoard :: NimBoard -> IO ()
putBoard [a,b,c,d,e] = do putRow 1 a
                          putRow 2 b
                          putRow 3 c
                          putRow 4 d
                          putRow 5 e

getDigit :: String -> IO Int
getDigit prompt = do putStr' prompt
                     x <- getChar
                     newline
                     if isDigit x then
                        return (digitToInt x)
                     else
                        do putStrLn' "ERROR: Invalid digit"
                           getDigit prompt

newline :: IO ()
newline = putChar '\n'


playNim :: NimBoard -> Int -> IO ()
playNim board player =
   do newline
      putBoard board
      if finished board then
         do newline
            putStr' "Player "
            putStr' (show (next player))
            putStrLn' " wins!!"
      else
         do newline
            putStr' "Player "
            putStrLn' (show player)
            row <- getDigit "Enter a row number: "
            num <- getDigit "Starts to remove: "
            if valid board row num then
               playNim (move board row num) (next player)
            else
               do newline
                  putStrLn' "ERROR: Invalid move"
                  playNim board player

nim :: IO ()
nim = playNim initial 1


-- 10.8 Game of Life
cls :: IO ()
cls = putStr' "\ESC[2J"


type Pos = (Int,Int)


writeAt :: Pos -> String -> IO ()
writeAt p xs = do goto p
                  putStr xs


goto :: Pos -> IO ()
goto (x,y) = putStr' ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")


width :: Int
width = 20


height :: Int
height = 20


type GoLBoard = [Pos]


glider :: GoLBoard
glider = [(4,2), (2,3), (4,3), (3,4), (4,4)]


expOne :: GoLBoard
expOne = [(1,1), (1,2), (1,3), (1,4), (2,1)]


showCells :: GoLBoard -> IO ()
showCells b = sequence_ [writeAt p "0" | p <- b]


isAlive :: GoLBoard -> Pos -> Bool
isAlive b p = elem p b


isEmpty :: GoLBoard -> Pos -> Bool
isEmpty b p = not (isAlive b p)


neighbs :: Pos -> [Pos]
neighbs (x,y) = map wrap [(x - 1,y - 1), (x, y - 1),
                          (x + 1, y - 1), (x - 1, y),
                          (x + 1, y), (x - 1, y + 1),
                          (x, y + 1), (x + 1, y + 1)]

wrap :: Pos -> Pos
wrap (x,y) = (((x - 1) `mod` width) + 1,
              ((y - 1) `mod` height) + 1)


liveNeighbs :: GoLBoard -> Pos -> Int
liveNeighbs b = length . filter (isAlive b) . neighbs


survivors :: GoLBoard -> [Pos]
survivors b = [p | p <- b, elem (liveNeighbs b p) [2,3]]

-- checks every position of the GoLBoard
-- births :: GoLBoard -> [Pos]
-- births b = [(x,y | x <- [1..width],
--                         [1..height,
--                         isEmpty b (x,y),
--                         liveNeighbs b (x,y) == 3])]


births :: GoLBoard -> [Pos]
births b = [p | p <- rmdups (concat (map neighbs b)),
           isEmpty b p,
           liveNeighbs b p == 3]


nextGen :: GoLBoard -> GoLBoard
nextGen b = survivors b ++ births b


life :: GoLBoard -> IO ()
life b = do cls
            showCells b
            wait 500000
            life (nextGen b)

wait :: Int -> IO ()
wait n = sequence_ [return () | _ <- [1..n]]
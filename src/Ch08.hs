module Ch08
  ( Pos
  , Trans
  , Pair
  , Assoc
  , findAssoc
  , Move (..)
--  , North
--  , South
--  , East
--  , West
  , move
  , moves
  , rev
  ) where

import Debug.Trace

debug = flip trace

type Pos = (Int,Int)

type Trans = Pos -> Pos

type Pair a = (a,a)

type Assoc k v = [(k,v)]

findAssoc :: Eq k => k -> Assoc k v -> v
findAssoc k t = head [v | (k',v) <- t,  k == k']

data Move = North | South | East | West deriving Show

move :: Move -> Pos -> Pos
move North (x,y) = (x,y + 1)
move South (x,y) = (x,y - 1)
move East  (x,y) = (x + 1,y)
move West  (x,y) = (x - 1,y)

moves :: [Move] -> Pos -> Pos
moves [] p     = p
moves (m:ms) p = moves ms (move m p)

rev :: Move -> Move
rev North = South
rev South = North
rev East = West
rev West = East

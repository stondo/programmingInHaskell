module Ch08
  ( Pos
  , Trans
  , Pair
  , Assoc
  , findAssoc
  , Move (..)
  , move
  , moves
  , rev
  , Nat (..)
  , nat2int
  , int2nat
  , addNat
  , addNat'
  , List (..)
  , len
  , Tree (..)
  , t
  , occurs
  , flatten
  , occursInSearchTree
  , p1
  , p2
  , p3
  , p4
  , Subst
  , eval
  , vars
  , boolsComplex
  , bools
  , substs
  , isTaut
  , Expr (..)
  , valueSimple
  , Op (..)
  , evalAbsMachine
  , exec
  , value
  , multNat
  , occursWithCompare
  , TreeWihtLeafValue (..)
  , numOfLeaves
  , tWihtLeafValue
  , balanced
  , balanced'
  , tWihtLeafValueUnbalanced
  , tWihtLeafValueOneBalance
  , splitList
  , balance
  , halveList
  , balanceShort
  , folde
  ) where

import Debug.Trace
import Ch07 (int2bin, rmdups)
import Ch01 (qsort)

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

data Shape = Circle Float | Rect Float Float deriving Show

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just (m `div` n)

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead xs = Just (head xs)

--newType Nat = N Int
data Nat = Zero | Succ Nat deriving Show

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))

addNat :: Nat -> Nat -> Nat
addNat Zero n  = n
addNat (Succ m) n = Succ (addNat m n)


addNat' :: Nat -> Nat -> Nat
addNat' m n = int2nat (nat2int m + nat2int n)


data List a = Nil | Cons a (List a) deriving Show

len :: List a -> Int
len Nil         = 0
len (Cons _ xs) = 1 + len xs


data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5
         (Node (Leaf 6) 7 (Leaf 9))

occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y)     = x == y
occurs x (Node l y r) = x == y || occurs x l || occurs x r

flatten :: Tree a -> [a]
flatten (Leaf x)     = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

occursInSearchTree :: Ord a => a -> Tree a -> Bool
occursInSearchTree x (Leaf y)                  = x == y
occursInSearchTree x (Node l y r) | x == y     = True
                                  | x < y      = occursInSearchTree x l
                                  | otherwise  = occursInSearchTree x r

-- other Tree's definition
-- data Tree a = Leaf a | Node (Tree a) (Tree a)
--
-- data Tree a = Leaf | Node (Tree a) a (Tree a)
--
-- data Tree a b = Leaf a | Node (Tree a b) b (Tree a b)
--
-- data Tree a = Node a (Tree a)

-- 8.6 tautology checker

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')


type Subst = Assoc Char Bool

eval :: Subst -> Prop -> Bool
eval _ (Const b)   = b
eval s (Var x)     = findAssoc x s
eval s (Not p)     = not (eval s p)
eval s (And p q)   = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q

vars :: Prop -> [Char]
vars (Const _)   = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

boolsComplex :: Int -> [[Bool]]
boolsComplex n = map (reverse . map conv . make n . int2bin) range
                 where
                    range     = [0..(2^n-1)]
                    make n bs = take n (bs ++ repeat 0)
                    conv 0    = False
                    conv 1    = True

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
          where bss = bools (n - 1)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
          where vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

-- 8.7 Abstract Machine

data Expr = Val Int | Add Expr Expr

valueSimple :: Expr -> Int
valueSimple (Val n)   = n
valueSimple (Add x y) = valueSimple x + valueSimple y

type Cont = [Op]

data Op = EVAL Expr | ADD Int

evalAbsMachine :: Expr -> Cont -> Int
evalAbsMachine (Val n)   c = exec c n
evalAbsMachine (Add x y) c = evalAbsMachine x (EVAL y : c)

exec :: Cont -> Int -> Int
exec []     n = n
exec (EVAL y : c) n = evalAbsMachine y (ADD n : c)
exec (ADD n : c)  m = exec c (n + m)

value :: Expr -> Int
value e = evalAbsMachine e []

-- 8.9 Exercises

-- 1.
multNat _ Zero        = Zero
multNat m (Succ n)    = addNat m (multNat m n)

-- multNat n m = int2nat (nat2int n * nat2int m)

-- 2.
occursWithCompare :: Ord a => a -> Tree a -> Bool
occursWithCompare x (Leaf v)     = x == v
occursWithCompare x (Node l v r) = case compare x v of
                                     LT -> occursWithCompare x l
                                     EQ -> True
                                     GT -> occursWithCompare x r

-- 3.
data TreeWihtLeafValue a = LeafVal a | NodeBranch (TreeWihtLeafValue a) (TreeWihtLeafValue a) deriving Show

numOfLeaves :: TreeWihtLeafValue a -> Int
numOfLeaves (LeafVal _)      = 1
numOfLeaves (NodeBranch l r) = numOfLeaves l + numOfLeaves r

tWihtLeafValue :: TreeWihtLeafValue Int
tWihtLeafValue = NodeBranch (NodeBranch (LeafVal 1) (LeafVal 4)) (NodeBranch (LeafVal 6) (LeafVal 9))

tWihtLeafValueOneBalance :: TreeWihtLeafValue Int
tWihtLeafValueOneBalance = NodeBranch (NodeBranch (LeafVal 1) (NodeBranch (LeafVal 2) (LeafVal 4))) (NodeBranch (LeafVal 6) (LeafVal 9))

tWihtLeafValueUnbalanced :: TreeWihtLeafValue Int
tWihtLeafValueUnbalanced = NodeBranch (NodeBranch (LeafVal 1) (NodeBranch (LeafVal 2) (NodeBranch (LeafVal 3) (LeafVal 4)))) (NodeBranch (LeafVal 11) (LeafVal 12))

balanced :: TreeWihtLeafValue a -> Bool
balanced (LeafVal _)                                               = True
balanced (NodeBranch l r) | numOfLeaves l == numOfLeaves r         = True
                          | numOfLeaves l + 1 - numOfLeaves r == 0 = True
                          | numOfLeaves r + 1 - numOfLeaves l == 0 = True
                          | otherwise                              = False

balanced' :: TreeWihtLeafValue a -> Bool
balanced' (LeafVal _) = True
balanced' (NodeBranch l r) = abs (numOfLeaves l - numOfLeaves r) <= 1 && balanced l && balanced r


-- 4.
splitList :: [a] -> ([a],[a])
splitList xs = (take middle xs, drop middle xs)
  where middle = (length xs) `div` 2

balance :: [a] -> TreeWihtLeafValue a
balance (x:xs) | null xs        = LeafVal x
balance (x:xs) | length xs == 1 = (NodeBranch (LeafVal x) (LeafVal (head xs)))
balance (x:xs) | length xs > 1  = (NodeBranch (balance fs) (balance ss))
  where (fs, ss) = splitList (x:xs)

halveList :: [a] -> ([a],[a])
halveList xs = splitAt (length xs `div` 2) xs

balanceShort :: [a] -> TreeWihtLeafValue a
balanceShort [x] = LeafVal x
balanceShort xs  = NodeBranch (balanceShort fs) (balanceShort ss)
  where (fs, ss) = halveList xs

-- 5.

--  data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val n)    = f n
folde f g (Add e e') = 
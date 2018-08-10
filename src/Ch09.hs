module Ch09
  ( Op (..)
  , valid
  , apply
  , Expr (..)
  , values
  , eval
  , subs
  , interleave
  , perms
  , choices
  , solution
  , split
  , exprs
  , combine
  , ops
  , solutions
  ) where


data Op = Add | Sub | Mul | Div

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"


valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub n m = n > m
valid Mul _ _ = True
valid Div n m = n `mod` m == 0


apply :: Op -> Int -> Int -> Int
apply Add n m = n + m
apply Sub n m = n - m
apply Mul n m = n * m
apply Div n m = n `div` m


data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  show (Val n) = show n
  show (App o l r) = brak l ++ show o ++ brak r
                     where
                       brak (Val n) = show n
                       brak e       = "(" ++ show e ++ ")"

-- *Main Ch01 Ch02 Ch03 Ch04 Ch05 Ch06 Ch07 Ch08 Ch09 Lib> show (App Ch09.Add (Ch09.Val 1) (App Ch09.Add (Ch09.Val 2) (Ch09.Val 3)))
-- "1+(2+3)"
-- *Main Ch01 Ch02 Ch03 Ch04 Ch05 Ch06 Ch07 Ch08 Ch09 Lib> show (App Ch09.Add (Ch09.Val 1) (App Mul (Ch09.Val 2) (Ch09.Val 3)))
-- "1+(2*3)"

values :: Expr -> [Int]
values (Val n)     = [n]
values (App _ l r) = values l ++ values r


eval :: Expr -> [Int]
eval (Val n)      = [n | n > 0]
eval (App op l r) = [apply op n m | n <- eval l,
                                    m <- eval r,
                                    valid op n m]

subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)


perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))


choices :: [a] -> [[a]]
choices = concat . map perms . subs


solution :: Expr -> [Int] -> Int -> Bool
solution e ns n =
  elem (values e) (choices ns) && eval e == [n]

-- 9.5
split :: [a] -> [([a],[a])]
split []     = []
split [_]    = []
split (x:xs) = ([x], xs) : [(x:ls,rs) | (ls,rs) <- split xs]


exprs :: [Int] -> [Expr]
exprs []  = []
exprs [n] = [Val n]
exprs ns = [e | (ls,rs) <- split ns,
                      l <- exprs ls,
                      r <- exprs rs,
                      e <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]


ops :: [Op]
ops = [Add, Sub, Mul, Div]


solutions :: [Int] -> Int -> [Expr]
solutions ns n =
  [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]
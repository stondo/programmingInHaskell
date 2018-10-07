module Ch12Spec
  (
  inc,
  inc',
  sqr
  sqr',
  (T ..)
  ) where


inc :: [Int] -> [Int]
inc []     = []
inc (n:ns) = n + 1 : inc ns

sqr :: [Int] -> [Int]
sqr []     = []
sqr (n:ns) = n^2 : sqr ns

inc' = map (+1)

sqr' = map (^2)

data T a = L a | N (T a) (T a) deriving Show

instance Functor T where
  -- fmap :: (a -> b) -> T a -> T b
  fmap g (L x)   = L (g x)
  fmap g (N l r) = N fmap (g l) (fmap (g r))
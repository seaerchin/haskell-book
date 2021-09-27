module Chap28 where

-- difference lists

newtype DList a = DL {unDL :: [a] -> [a]}

empty :: DList a
empty = DL id

singleton :: a -> DList a
singleton x = DL ([x] ++)

infixr 9 `cons`

cons :: a -> DList a -> DList a
cons x xs = DL ((x :) . unDL xs)

infixl 9 `snoc`

snoc :: DList a -> a -> DList a
snoc dl a = DL ((\x -> x ++ [a]) . unDL dl)

append :: DList a -> DList a -> DList a
append x y =
  -- DL $ (\fy -> fy . unDL x) $ unDL y
  let lx = unDL x
      ly = unDL y
   in DL (lx . ly)

x = singleton 1

y = singleton 2 `snoc` 3 `snoc` 4

toList x = unDL x []

instance (Show a) => Show (DList a) where
  show dl = show (unDL dl [])

schlemiel :: Int -> [Int]
schlemiel i = go i []
  where
    go 0 xs = xs
    go n xs = go (n -1) (n : xs)

constructDlist :: Int -> [Int]
constructDlist i = toList $ go i empty
  where
    go 0 xs = xs
    go n xs = go (n -1) (singleton n `append` xs)

data Queue a = Queue
  { enqueue :: [a],
    dequeue :: [a]
  }
  deriving (Eq, Show)

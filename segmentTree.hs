data SegTree m = Leaf m | Node m (SegTree m) (SegTree m) deriving (Eq, Show)

val :: SegTree m -> m
val (Leaf x) = x
val (Node x _ _) = x

fromList' :: Semigroup a => Int -> [a] -> SegTree a
fromList' 1 [x] = Leaf x
fromList' n xs = Node (val left <> val right) left right
  where
    m = n `div` 2
    (lxs, rxs) = splitAt m xs
    left = fromList' m lxs
    right = fromList' (n - m) rxs

fromList :: Semigroup a => [a] -> (Int, SegTree a)
fromList xs = (size, fromList' size xs)
  where size = length xs

query :: Semigroup a => (Int, SegTree a) -> (Int, Int) -> a
query (n, (Leaf x)) range = x
query (n, (Node x left right)) (i, j)
  | (i, j) == (1, n) = x
  | j <= m = query (m, left) (i, j)
  | i > m = query ((n - m), right) (i - m, j - m)
  | otherwise = query (m, left) (i, m) <> query ((n - m), right) (1, j - m)
    where
      m = n `div` 2

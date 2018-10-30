data BinarySearchTree a = Null | Node Int a (BinarySearchTree a) (BinarySearchTree a)
  deriving (Eq, Show)

fromList' 0 [] = Null
fromList' 1 [(idx, x)] = Node idx x Null Null
fromList' n xs = Node idx x left right
  where
    m = n `div` 2
    (lxs, ((idx, x):rxs)) = splitAt m xs
    left = fromList' m lxs
    right = fromList' (n - m - 1) rxs

fromList xs = fromList' len txs
  where
    len = length xs
    txs = zip [1..] xs

query Null idx = Nothing
query (Node i x left right) idx
  | i == idx = Just x
  | idx > i = query right idx
  | idx < i = query left idx

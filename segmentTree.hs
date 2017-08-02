data Range = Range Int Int deriving (Eq, Show)

getLeft (Range l _) = l
atom (Range l r) = l == r
include (Range l1 r1) (Range l2 r2) = l1 <= l2 && r2 <= r1
disjoint (Range l1 r1) (Range l2 r2) = r1 < l2 || r2 < l1
splitRange (Range l r) = (Range l r1, Range (r1 + 1) r)
  where r1 = (r + l) `div` 2

splitList xs = splitAt ((1 + length xs) `div` 2) xs

data SegTree a = Empty | Node {
    left :: SegTree a,
    right :: SegTree a,
    getRange :: Range,
    getValue :: a
  } deriving Show

getValIn :: (a -> a -> a) -> Range -> SegTree a -> a
getValIn fn range (Node leftTree rightTree treeRange value)
  | range `include` treeRange = value
  | disjoint leftRange range = rightVal
  | disjoint rightRange range = leftVal
  | otherwise = fn leftVal rightVal
    where
      leftRange = getRange leftTree
      rightRange = getRange rightTree
      leftVal = getValIn fn range leftTree
      rightVal = getValIn fn range rightTree

buildSegTree' :: (a -> a -> a) -> Range -> [a] -> SegTree a
buildSegTree' fn range xs
  | atom range = Node Empty Empty range (head xs)
  | otherwise = Node leftTree rightTree range (fn leftVal rightVal)
    where
      (leftRange, rightRange) = splitRange range
      (leftX, rightX) = splitList xs
      leftTree = buildSegTree' fn leftRange leftX
      rightTree = buildSegTree' fn rightRange rightX
      leftVal = getValue leftTree
      rightVal = getValue rightTree

buildSegTree :: (a -> a -> a) -> [a] -> SegTree a
buildSegTree fn xs = buildSegTree' fn (Range 1 (length xs)) xs

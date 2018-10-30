-- compare get a index and return isMatch. compare l == True and compare r == False
-- finally returns m which compare m == True and compare (m + 1) == False

binarySearch' :: Integral a => (a -> Bool) -> a -> a -> Maybe a
binarySearch' compare l r
  | r - l == 1 = Just l
  | compare m = binarySearch' compare m r
  | otherwise = binarySearch' compare l m
    where
      m  = (r + l) `div` 2

binarySearch compare l r
  | compare r = Just r
  | not (compare l) = Nothing
  | otherwise = binarySearch' compare l r

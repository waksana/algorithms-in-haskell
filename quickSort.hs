classify :: (a -> Bool) -> [a] -> ([a], [a])
classify f [] = ([], [])
classify f (x:xs) = if f x then (x:l, r) else (l, x:r)
  where
    (l, r) = classify f xs

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort l ++ (x:quickSort r)
  where
    (l, r) = classify (\a -> a < x) xs

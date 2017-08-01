--https://www.twanvl.nl/blog/haskell/Knuth-Morris-Pratt-in-Haskell

data KMP a = KMP {done :: Bool, next :: a -> KMP a}

isSubStrOf str pattern = match (makeTable pattern) str
  where
    match table [] = done table
    match table (c:str) = done table || match (next table c) str

makeTable :: Eq a => [a] -> KMP a
makeTable xs = table
  where table = makeTable' xs (const table)

makeTable' [] failure = KMP True failure
makeTable' (x:xs) failure = KMP False test
  where test c = if c == x then success else failure c
        success = makeTable' xs (next (failure x))

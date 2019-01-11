
data Tree a = Node a [Tree a]

splitN' l (r:rs) 1 = (l, r, rs)
splitN' l (r:rs) n = splitN' (r:l) rs (n - 1)

conc [] r = r
conc (l:ls) r = conc ls (l:r)

child (path, (Node v childs)) n = ((v, l, r):path, c)
  where
    (l, c, r) = splitN' [] childs n


changev (path, (Node v childs)) x = (path, (Node x childs))

visitParent (((v, l, r):path), c) = (path, Node v (conc l (c:r)))

visitLeft (((v, (nt:l), r):path), tree) = (((v, l, (tree:r)):path), nt)

visitRight (((v, l, (nt:r)):path), tree) = (((v, (tree:l), r):path), nt)

insertLeft (((v, l, r):path), tree) x = (((v, (Node x []):l, r):path), tree)
insertRight (((v, l, r):path), tree) x = (((v, l, (Node x []):r):path), tree)

insertChild (path, (Node a childs)) x = (path, (Node a ((Node x []):childs)))

delete (((v, l, r):path), c) = (path, Node v (conc l r))

p (path, (Node v _)) = v

processCmd ["print"] zipper = do
  putStrLn $ show $ p zipper
  return zipper

processCmd ["change", x] zipper = do
  let n = (read::String -> Int) x
  return $ changev zipper n

processCmd ["visit", "left"] zipper = do
  return $ visitLeft zipper
processCmd ["visit", "right"] zipper = do
  return $ visitRight zipper
processCmd ["visit", "parent"] zipper = do
  return $ visitParent zipper
processCmd ["visit", "child", x] zipper = do
  let n = (read::String -> Int) x
  return $ child zipper n
processCmd ["delete"] zipper = do
  return $ delete zipper

processCmd ["insert", "child", x] zipper = do
  let n = (read::String -> Int) x
  return $ insertChild zipper n
processCmd ["insert", "left", x] zipper = do
  let n = (read::String -> Int) x
  return $ insertLeft zipper n
processCmd ["insert", "right", x] zipper = do
  let n = (read::String -> Int) x
  return $ insertRight zipper n

handle 0 _ = return ()
handle n zipper = do
  cmds <- words <$> getLine
  newZipper <- processCmd cmds zipper
  handle (n - 1) newZipper

main = do
  n <- (read::String -> Int) <$> getLine
  handle n ([], Node 0 [])

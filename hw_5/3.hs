module Permute where
import System.Random
-- try in case of problems
-- $ cabal install --lib random

randomMerge :: IO [a] -> IO [a] -> IO [a]
randomMerge rxs rys = do
    xs <- rxs
    ys <- rys
    randomMerge' (length xs, xs) (length ys, ys)
  where
    randomMerge' (0,  _) (_, ys)  = return ys
    randomMerge' (_, []) (_, ys)  = return ys
    randomMerge' (_, xs) (0,  _)  = return xs
    randomMerge' (_, xs) (_, [])  = return xs
    randomMerge' (nx, x:xs) (ny, y:ys) = do
      k <- randomRIO (1, nx + ny)
      if k <= nx then
        fmap (x:) (randomMerge' (nx - 1, xs) (ny, y:ys))
      else
        fmap (y:) (randomMerge' (nx, x:xs)   (ny - 1, ys))

permute :: [a] -> IO [a]
permute []   = return []
permute [x]  = return [x]
permute xs   = randomMerge (permute l) (permute r)
  where (l, r) = splitAt (div (length xs) 2) xs


-- Examples
{-
*Permute> permute []
[]
*Permute> permute [4]
[4]
*Permute> permute [1..10]
[4,9,5,8,2,3,10,1,6,7]
*Permute> permute [Just 1, Just 6, Nothing, Just 7]
[Just 1,Just 7,Just 6,Nothing]
-}

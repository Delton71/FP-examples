import System.Environment
import Control.Monad


insertBy' :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertBy' _ x [] = [x]
insertBy' cmp x (y:ys)
  = case cmp x y of
    GT -> y : insertBy' cmp x ys
    _  -> x : (y:ys)

sortBy' :: (a -> a -> Ordering) -> [a] -> [a]
sortBy' cmp = foldr (insertBy' cmp) []

isDescSort :: Ord a => Bool -> [a] -> [a]
isDescSort flag
  | flag       = sortBy' (flip compare)
  | otherwise  = sortBy' compare

main :: IO ()
main = do 
  args <- getArgs
  when (length args /= 3) $ 
    error "3 arguments were expected: input, output, (asc/desc :: Bool)!"
  let flag = read (args !! 2) :: Bool
  writeFile (args !! 1) . unlines . isDescSort flag . lines 
    =<< readFile (args !! 0)

-- Examples
{-
$ ./sortFile input.txt output.txt True
*** action ***
-}

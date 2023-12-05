takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' c l = foldr (\x xs -> if c x then x:xs else []) [] l

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' c [] = []
dropWhile' c (x:xs)
  | c x       = dropWhile' c xs
  | otherwise = x:xs

-- for gigachads
dropWhile'' :: (a -> Bool) -> [a] -> [a]
dropWhile'' c l = foldr (\x xs is_dropping ->
  if is_dropping && c x then xs True else x:xs False) (const []) l True


-- Examples
{-
*Main> takeWhile' (<15) [1..10]
[1,2,3,4,5,6,7,8,9,10]
*Main> takeWhile' (>15) [1..10]
[]
*Main> takeWhile' (<5) [1..10]
[1,2,3,4]
*Main> takeWhile' (>5) [1..10]
[]
*Main> takeWhile' (\_ -> True) [1..]
[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,...

*Main> dropWhile' (<15) [1..10]
[]
*Main> dropWhile' (>15) [1..10]
[1,2,3,4,5,6,7,8,9,10]
*Main> dropWhile' (<5) [1..10]
[5,6,7,8,9,10]
*Main> dropWhile' (>5) [1..10]
[1,2,3,4,5,6,7,8,9,10]
*Main> dropWhile' (<10) [1..]
[10,11,12,13,14,15,16,17,18,19,20,21,22,23,...

*Main> dropWhile'' (<10) [1..]
[10,11,12,13,14,15,16,17,18,19,20,21,22,23,...
-}

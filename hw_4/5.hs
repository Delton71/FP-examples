remDups :: Eq a => [a] -> [a]
remDups [] = []
remDups (x:xs) = x : remDups (filter (/= x) xs)

-- Examples
{-
*Main> remDups [1, 1, 1, 1, 1]
[1]
*Main> remDups [1, 2, 3, 4, 5, 4, 3, 2, 1, 2, 3, 4, 5]
[1,2,3,4,5]
*Main> remDups ['a', 'c', 'b', 'c', 'a']
"acb"
*Main> remDups [1, 2, 3, 4, 5]
[1,2,3,4,5]
*Main> remDups [1, 2, 3, 5, 2, 3, 6, 4, 6, 7, 1, 7, 8, 4, 9, 2, 10, 11, 4]
[1,2,3,5,6,4,7,8,9,10,11]
*Main> remDups []
[]
-}

-- -----/ a) using iterate /-----

rotts :: [a] -> [[a]]
rotts l = take (length l) (iterate (\(x:xs) -> xs ++ [x]) l)

-- -----/ b) using scanl /-----

rotts' :: [a] -> [[a]]
rotts' l = take (length l) (scanl (\(x:xs) y -> xs ++ [y]) l l)


-- Examples
{-
*Main> rotts [1, 2, 3]
[[1,2,3],[2,3,1],[3,1,2]]
*Main> rotts "hello"
["hello","elloh","llohe","lohel","ohell"]
*Main> rotts "a"
["a"]
*Main> rotts ""
[]

*Main> rotts' [1, 2, 3]
[[1,2,3],[2,3,1],[3,1,2]]
*Main> rotts' "hello"
["hello","elloh","llohe","lohel","ohell"]
*Main> rotts' "a"
["a"]
*Main> rotts' ""
[]
-}

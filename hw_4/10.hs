prefixes' :: [a] -> [[a]]
prefixes' l = foldr (\x -> map (x:) . ([[]] ++)) [] l

-- Examples
{-
*Main> prefixes' "hello"
["h","he","hel","hell","hello"]
*Main> prefixes' [1, 2, 3, 4, 5]
[[1],[1,2],[1,2,3],[1,2,3,4],[1,2,3,4,5]]
*Main> prefixes' [1..]
[[1],[1,2],[1,2,3],[1,2,3,4],[1,2,3,4,5],[1,2,3,4,5,6],...
*Main> prefixes' []
[]
-}

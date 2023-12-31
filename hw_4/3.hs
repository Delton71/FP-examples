segsn :: Int -> [a] -> [[a]]
segsn n l = take (length l - n + 1) $ segsn' n l
  where segsn' _ [] = [[]]
        segsn' n (x:xs) = take n (x:xs) : segsn' n xs

segs :: [a] -> [[a]]
segs l =  concat [segsn n l | n <- [1..(length l)]]

-- Examples
{-
*Main> segs "abc"
["a","b","c","ab","bc","abc"]

*Main> segs "hello"
["h","e","l","l","o","he","el","ll","lo",
"hel","ell","llo","hell","ello","hello"]

*Main> segs "0123456789"
["0","1","2","3","4","5","6","7","8","9",
"01","12","23","34","45","56","67","78","89",
"012","123","234","345","456","567","678","789",
"0123","1234","2345","3456","4567","5678","6789",
"01234","12345","23456","34567","45678","56789",
"012345","123456","234567","345678","456789",
"0123456","1234567","2345678","3456789",
"01234567","12345678","23456789",
"012345678","123456789",
"0123456789"]
-}

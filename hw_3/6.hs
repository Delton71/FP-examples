-- -----/ Amicable numbers /-----

sumDivs :: Integer -> Integer
sumDivs n = sum [x | x <- [1..(div n 2)], mod n x == 0]

isAmicable :: Integer -> Integer -> Bool
isAmicable x y
  | x < 0 || y < 0 = error "arguments cannot be negative number!"
  | otherwise = (sumDivs x == y) && (sumDivs y == x)


-- examples

x     = isAmicable 1184 1210
-- True

x'    = isAmicable 1184 1211
-- False

x''   = isAmicable 0 0
-- True

x'''  = isAmicable 220 284
-- True

x'''' = isAmicable 3 12
-- False


import GHC.Float

-- -----/ a) f :: (b -> c, a -> b) -> a -> с /-----

f :: (t2 -> t3, t1 -> t2) -> t1 -> t3
f = \p x -> fst p (snd p (x))

-- -----/ b) [Double -> Double] -> Int -> Int /-----

g :: [Double -> Double] -> Int -> Int
g = \l x -> double2Int $
  ((l ++ [\y -> z / y | z <- [1..]]) !! x) 1.0

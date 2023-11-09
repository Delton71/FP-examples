-- -----/ curry & uncurry /-----

{-
  f :: a -> b -> c -- \equiv f :: a -> (b -> c)
  g :: (a, b) -> c
  -- f = curry g = uncurry f
-}

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f x y = f (x, y)
uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f (x,y) = f x y


-- examples

l = [(x, y) | x <- [1..], y <- [1..]] :: [(Integer, Integer)]
c = take 10 $ map (uncurry' (+)) l
-- c = [2,3,4,5,6,7,8,9,10,11]

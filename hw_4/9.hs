import Data.Maybe

foldr1' :: Foldable t => (a -> a -> a) -> t a -> a
foldr1' f xs = fromMaybe (error "foldr1': empty argument!")
  (foldr mf Nothing xs)
  where mf x Nothing  = Just x
        mf x (Just y) = Just (f x y)

lmax :: Ord a => [a] -> a
lmax = foldr1' max


-- Examples
{-
foldr1 (+) [1..5]
15
*Main> foldr1' (+) []
*** Exception: foldr1': empty argument!
*Main> foldr1' (+) Nothing
*** Exception: foldr1': empty argument!
*Main> foldr1' (+) [1..]
-- "forever" --

*Main> lmax [1, 2, 3, 4, 5]
5
*Main> lmax [5, 3, 4, 6, 2, 7, 3, 5, 4]
7
*Main> lmax [-5, -3, -4, -6, -2, -7, -3, -5, -4]
-2
-}

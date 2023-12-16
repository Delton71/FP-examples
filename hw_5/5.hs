import Data.Foldable (foldl')

average :: (Foldable f, Fractional a) => f a -> Maybe a
average xs = avg' $ foldl' step (0, 0) xs
  where avg' (a, n) 
          | n == 0    = Nothing
          | otherwise = Just (a / fromIntegral n)
        step (a, n) x = (a + x, n + 1)

-- Examples
{-
*Main> average [1..9]
Just 5.0
*Main> average [10, 10, 10]
Just 10.0
*Main> average []
Nothing
*Main> average [1, 6, 2, 1, 4]
Just 2.8
*Main> average (Just 7.7)
Just 7.7
-}

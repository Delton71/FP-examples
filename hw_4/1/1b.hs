-- -----/ b) A ~ (A, ()) /-----

f :: a -> (a, ())
f x = (x, ())

g :: (a, ()) -> a
g (x, ()) = x

-- Examples
{-
*Main> :t (f . g)
(f . g) :: (a, ()) -> (a, ())
*Main> :t (g . f)
(g . f) :: c -> c

*Main> (g . f) $ 4
4
*Main> (g . f) $ (Left 5)
Left 5
*Main> (g . f) $ "abcd"
"abcd"
*Main> (f . g) $ (4, ())
(4,())
*Main> (f . g) $ (Left 5, ())
(Left 5,())
*Main> (f . g) $ ("abcd", ())
("abcd",())
-}

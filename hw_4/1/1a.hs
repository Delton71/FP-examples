-- -----/ a) Ordering ~ Either () Bool /-----

f :: Ordering -> Either () Bool
f LT = Right False
f EQ = Left ()
f GT = Right True

g :: Either () Bool -> Ordering
g (Right False) = LT
g (Left ()) = EQ
g (Right True) = GT

-- Examples
{-
*Main> :t (f . g)
(f . g) :: Either () Bool -> Either () Bool
*Main> :t (g . f)
(g . f) :: Ordering -> Ordering

*Main> (f . g) $ compare 4 5
LT
*Main> (f . g) $ compare 5 4
GT
*Main> (f . g) $ compare 5 5
EQ
*Main> (g . f) $ (Right False)
Right False
*Main> (g . f) $ (Right True)
Right True
*Main> (g . f) $ (Left ())
Left ()
-}

-- -----/ c) (Maybe A, Maybe B) ~ 
--         ~ Maybe (Either (A, B) (Either A B)) /-----

f :: (Maybe a, Maybe b) -> Maybe (Either (a, b) (Either a b))
f (Just x, Just y) = Just (Left (x, y))
f (Just x, Nothing) = Just (Right (Left x))
f (Nothing, Just y) = Just (Right (Right y))
f (Nothing, Nothing) = Nothing

g :: Maybe (Either (a, b) (Either a b)) -> (Maybe a, Maybe b)
g Nothing = (Nothing, Nothing)
g (Just (Left (x, y))) = (Just x, Just y)
g (Just (Right (Left x))) = (Just x, Nothing)
g (Just (Right (Right y))) = (Nothing, Just y)

-- Examples
{-
*Main> :t (f . g)
(f . g)
  :: Maybe (Either (a, b) (Either a b))
     -> Maybe (Either (a, b) (Either a b))
*Main> :t (g . f)
(g . f) :: (Maybe a, Maybe b) -> (Maybe a, Maybe b)

*Main> (f . g) $ Nothing
Nothing
*Main> (f . g) $ Just (Left (4, "abcd"))
Just (Left (4,"abcd"))
*Main> (f . g) $ Just (Right (Left 4))
Just (Right (Left 4))
*Main> (f . g) $ Just (Right (Right "abcd"))
Just (Right (Right "abcd"))

*Main> (g . f) $ (Just 4, Just "abcd")
(Just 4,Just "abcd")
*Main> (g . f) $ (Nothing, Just "abcd")
(Nothing,Just "abcd")
*Main> (g . f) $ (Just 4, Nothing)
(Just 4,Nothing)
*Main> (g . f) $ (Nothing, Nothing)
(Nothing,Nothing)
-}

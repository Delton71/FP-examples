-- ($) :: (a -> b) -> a -> b
-- (.) :: (b -> c) -> (a -> b) -> a -> c

g' = \f -> f $ (.)
g = \f -> f $ ($) $ (.)

-- (type g') == (type g)

import Data.Foldable
-- using foldl and foldr:
-- -----/ a) map /-----

map' :: (a -> b) -> [a] -> [b]
map' f l = foldr (\x xs -> f x:xs) [] l


-- -----/ b) filter /-----

filter' :: (a -> Bool) -> [a] -> [a]
filter' c l = foldr (\x xs -> if c x then x:xs else xs) [] l


-- -----/ c) all /-----

all' :: Foldable t => (a -> Bool) -> t a -> Bool
all' c l = foldr ((&&) . c) True l
-- all' c l = foldl' (\x xs -> x && (c xs)) True l


-- -----/ d) any /-----

any' :: Foldable t => (a -> Bool) -> t a -> Bool
any' c l = foldr ((||) . c) False l
-- any' c l = foldl' (\x xs -> x || (c xs)) False l


-- Examples
{-
map' (+3) [1..5]
[4,5,6,7,8]
*Main Data.Char> map' (\x -> chr $ (ord x + ord 'A' - ord 'a')) "hello"
"HELLO"

*Main> filter (<3) [1..5]
[1,2]
*Main Data.Char> filter (\x -> ord 'a' <= ord x && ord x <= ord 'z') "hElLO"
"hl"

*Main> all' (<10) [1..9]
True
*Main> all' (<10) [1..10]
False

*Main> any' (<10) [10..20]
False
*Main> any' (<10) [9..20]
True
-}

import GHC.Float

-- -----/ a) /-----

-- C -> (A, B)
t :: Int -> (Float, Double)
t n = (int2Float n, int2Double  n)
-- (C -> A, C -> B)
k :: (Int -> Float, Int -> Double)
k = (int2Float, int2Double)

f :: (Int -> (Float, Double)) -> (Int -> Float, Int -> Double)
f t = (fst . t, snd . t)
g :: (Int -> Float, Int -> Double) -> (Int -> (Float, Double))
g k n = (fst k n, snd k n)

-- f . g
--  :: (Int -> Float, Int -> Double) -> (Int -> Float, Int -> Double)
-- g . f :: (Int -> (Float, Double)) -> Int -> (Float, Double)

-- f . g == id == g . f

-- -----/ b) /-----

-- C -> (B -> A)
t' :: Double -> (Float -> Int)
t' a = \b -> round a + round b
-- (B, C) -> A
k' :: (Float, Double) -> Int
k' (a, b) = round a + round b

f' :: (Double -> (Float -> Int)) -> ((Float, Double) -> Int)
f' t' = \(a, b) -> t' b a
g' :: ((Float, Double) -> Int) -> (Double -> (Float -> Int))
g' k' = \a b -> k' (b, a)

-- f' . g' :: ((Float, Double) -> Int) -> (Float, Double) -> Int
-- g' . f' :: (Double -> Float -> Int) -> Double -> Float -> Int

-- f' . g' == id == g' . f'

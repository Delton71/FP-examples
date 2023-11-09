-- -----/ define f :: A -> B -> C /-----

{-# LANGUAGE DerivingVia #-}

-- let types A, B, C, D:
newtype A = A { uA :: Integer }
  deriving Num via Integer
newtype B = B { uB :: Integer }
  deriving Num via Integer
newtype C = C { uC :: Integer }
  deriving Num via Integer
newtype D = D { uD :: Integer }
  deriving Num via Integer

g :: A -> B -> D
g _ _ = 0
h :: D -> C
h _ = 1

f = curry (h . uncurry g)
-- f :: A -> B -> C

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
import Data.Bits

-- -----/ a) Ring /-----

type Bfn = (Bool, Bool, Bool) -> Bool

sumBfn :: Bfn -> Bfn -> Bfn
sumBfn f g (x, y, z) = xor (f (x, y, z)) (g (x, y, z))

mltBfn :: Bfn -> Bfn -> Bfn
mltBfn f g (x, y, z) = f (x, y, z) && g (x, y, z)

eqBfn :: Bfn -> Bfn -> Bool
eqBfn f g = and ([(\ tr -> f tr == g tr) (x, y, z) |
  x <- [False, True], y <- [False, True], z <- [False, True]])

bfnZero :: Bfn
bfnZero (_, _, _) = False
bfnOne :: Bfn
bfnOne (_, _, _) = True

notBfn :: Bfn -> Bfn
notBfn f (x, y, z) = not (f (x, y, z))

printBfn :: Bfn -> String
printBfn f = show ([
    ((fromEnum x, fromEnum y, fromEnum z), fromEnum (f (x, y, z))) |
    x <- [False, True], y <- [False, True], z <- [False, True]
  ])

-- examples
f :: Bfn
f (a, b, c) = a && (b || c)
g (a, b, c) = a || b || c

s = sumBfn f g
m = mltBfn f g
e = eqBfn f g
e' = eqBfn (\(x, y, z) -> (x || y) && z) (\(x, y, z) -> x && z || y && z)
p = printBfn f
-- "[((0,0,0),0),((0,0,1),0),((0,1,0),0),((0,1,1),0),
--  ((1,0,0),0),((1,0,1),1),((1,1,0),1),((1,1,1),1)]"

-- -----/ b) Instance /-----

instance Eq Bfn where
  f == g = eqBfn f g

instance Num Bfn where
  f + g = sumBfn f g
  f - g = sumBfn f (notBfn g)
  f * g = mltBfn f g
  negate = notBfn
  abs f = bfnOne
  signum = id
  fromInteger n = if mod n 2 == 0 then bfnZero else bfnOne

instance Show Bfn where
  show f = unlines [
      show (fromEnum x, fromEnum y, fromEnum z) ++ 
            " " ++ show (fromEnum (f (x, y, z))) |
      x <- [False, True], y <- [False, True], z <- [False, True]
    ]
  -- show f = show ([((x, y, z), f (x, y, z)) |
  --   x <- [False, True], y <- [False, True], z <- [False, True]])

-- examples
sum_x = f + g
sub_x = f - g
mlt_x = f * g
neg_f = negate f
abs_f = abs f
sig_f = signum f

fi_x :: Bfn
fi_x = fromInteger 5

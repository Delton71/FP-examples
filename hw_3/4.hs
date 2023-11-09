-- -----/ f(n) = 0^{fib(n)} + 1^{fib(n)} + \ldots + n^{fib(n)} /-----

fibs :: Integer -> Integer
fibs 0 = 0
fibs 1 = 1
fibs x = fibs(x-1) + fibs(x-2)

f :: Integer -> Integer
f n
  | n < 0 = error "argument cannot be negative number!"
  | otherwise = (* fibs n) $ sum [0..n]

-- examples

g     = f 0   -- 0
g'    = f 1   -- 1
g''   = f 2   -- 3
g'''  = f 3   -- 12
g'''' = f 10  -- 3025
-- h = f (-3) -> error

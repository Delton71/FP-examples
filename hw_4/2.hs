{-
Source: 
https://github.com/edashkov/hse-fp-22/blob/55e59744d257f0a4590e909422feec599e9101cf/hse-fp-l3.hs
-}

data Nat = Z | S Nat
  deriving (Eq,Ord,Show)

nat2int :: (Integral a) => Nat -> a
nat2int Z = 0
nat2int (S n) = nat2int n + 1

int2nat :: (Integral a) => a -> Nat
int2nat n
  | n < 0      = error "negative value!"
  | n == 0     = Z
  | otherwise  = S (int2nat (n - 1))

natAdd :: Nat -> Nat -> Nat
natAdd m Z     = m
natAdd m (S n) = S (natAdd n m)

-- foldNat transforms every natural to an iterator
-- fold f x (S (S (S Z))) = f (f (f x))
foldNat :: (a -> a) -> a -> Nat -> a
foldNat f x Z = x
foldNat f x (S n) = f (foldNat f x n)

natMlt :: Nat -> Nat -> Nat
natMlt n m = foldNat (foldNat S m) Z n

natPred :: Nat -> Nat
natPred = snd . foldNat (\(n,_) -> (S n, n)) (Z,Z)

-- -----/ My code starts here /-----

natSub :: Nat -> Nat -> Nat
natSub = foldNat natPred

natDiv :: Nat -> Nat -> Nat
natDiv _ Z = error "divide by zero!"
natDiv n m
  | n < m      = Z
  | otherwise  = S (natDiv (natSub n m) m)

natMod :: Nat -> Nat -> Nat
natMod _ Z = error "divide by zero!"
natMod n m
  | n < m      = n
  | otherwise  = natMod (natSub n m) m

natEven :: Nat -> Bool
natEven n = natMod n (S (S Z)) == S Z
natOdd  :: Nat -> Bool
natOdd  n = natMod n (S (S Z)) == Z

data NatB = ZB | Db NatB | DbI NatB
  deriving (Eq,Ord,Show)

natb2int :: (Integral a) => NatB -> a
natb2int ZB       = 0
natb2int (Db n)   = 2 * natb2int n
natb2int (DbI n)  = 1 + 2 * natb2int n

int2natb :: (Integral a) => a -> NatB
int2natb n
  | n < 0      = error "negative value!"
  | n == 0     = ZB
  | even n     = Db  (int2natb (n `div` 2))
  | odd  n     = DbI (int2natb (n `div` 2))
  | otherwise  = error "smth wrong!"

natb2nat :: NatB -> Nat
natb2nat ZB = Z
natb2nat (Db n)  =         natMlt (S (S Z)) (natb2nat n)
natb2nat (DbI n) = natAdd (natMlt (S (S Z)) (natb2nat n)) (S Z)

nat2natb :: Nat -> NatB
nat2natb n
  | n == Z     = ZB
  | natEven n  = DbI (nat2natb (natDiv n (S (S Z))))
  | natOdd  n  = Db  (nat2natb (natDiv n (S (S Z))))
  | otherwise  = error "smth wrong!"

-- -----/ Testing /-----

f :: NatB -> NatB
f = nat2natb . natb2nat
intf :: (Integral a) => a -> a
intf = natb2int . f . int2natb

g :: Nat -> Nat
g = natb2nat . nat2natb
intg :: (Integral a) => a -> a
intg = nat2int . g . int2nat

-- Examples
{-
*Main> g (S (S (S Z)))
S (S (S Z))
*Main> f (Db (DbI (Db (DbI ZB))))
Db (DbI (Db (DbI ZB)))

*Main> natb2int . f . int2natb $ 4
4
*Main> natb2int . f . int2natb $ 5
5
*Main> natb2int . f . int2natb $ 6
6
*Main> natb2int . f . int2natb $ 7
7
*Main> natb2int . f . int2natb $ 0
0
*Main> natb2int . f . int2natb $ 1000
1000

*Main> nat2int . g . int2nat $ 0
0
*Main> nat2int . g . int2nat $ 1
1
*Main> nat2int . g . int2nat $ 2
2
*Main> nat2int . g . int2nat $ 10
10
*Main> nat2int . g . int2nat $ 1000
1000
-}

module Primes where

noPrimes y = noPrimes' 2 y []

noPrimes' :: Integer -> Int -> [Integer] -> [Integer]
noPrimes' x z xs =
  if length xs == z then xs
  else if isprime then noPrimes' (x+1) z ([x] ++ xs) else noPrimes' (x+1) z xs
  where sr = sqr x
        xss = filter (\y -> y <= sr) xs
        l1 = filter (\y -> (rem x y == 0)) xss
        isprime = length l1 == 0

primes y = primes' 2 y []

primes' :: Integer -> Integer -> [Integer] -> [Integer]
primes' x z xs =
  if x >= z then xs
  else if isprime then primes' (x+1) z (xs ++ [x]) else primes' (x+1) z xs
  where sr = sqr x
        xss = filter (\y -> y <= sr) xs
        l1 = filter (\y -> (rem x y == 0)) xss
        isprime = length l1 == 0

sqr :: Integer -> Integer
sqr x = toInteger $ ceiling $ sqrt (fromIntegral x)
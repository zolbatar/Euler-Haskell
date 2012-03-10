module Primes where

import Data.List

-- Sieve of Eratosthenes
primes :: Integer -> [Integer]
primes x = 2 : primes' [] [3,5..x] (sqr x)

primes' :: [Integer] -> [Integer] -> Integer -> [Integer]
primes' ps [] m = ps
primes' ps xs m = if x > m then (ps ++ xs) else primes' (x : ps) (filter (\y -> rem y x /= 0) xs) m
  where x = head xs

-- Recursive brute force algorithms...
noPrimes y = noPrimes' 2 y []
noPrimes' :: Integer -> Int -> [Integer] -> [Integer]
noPrimes' x z xs =
  if length xs == z then xs
  else if l1 == Nothing then noPrimes' (x+1) z ([x] ++ xs) else noPrimes' (x+1) z xs
  where sx = sqr x
        l1 = find (\y -> y <= sx && (rem x y == 0)) xs

-- Square root helpers
sqr :: Integer -> Integer
sqr x = toInteger $ ceiling $ sqrt (fromIntegral x)

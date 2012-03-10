module Primes where

import Data.List

-- Sieve of Eratosthenes
primesFast :: [Int] -> Int -> Int -> [Int]
primesFast xs x m = 
  if x == l then xs
  else primesFast (xs \\ [y*2,y*3..m]) (x+1) m
  where y = xs !! x
        l = length xs

primesFast2 :: [Int] -> [Int] -> Int -> [Int]
primesFast2 xs [] m = xs

-- Crude, recursive brute force algorithms...okay for small sets, but exponentially worse for >100000
noPrimes y = noPrimes' 2 y []
noPrimes' :: Integer -> Int -> [Integer] -> [Integer]
noPrimes' x z xs =
  if length xs == z then xs
  else if l1 == Nothing then noPrimes' (x+1) z ([x] ++ xs) else noPrimes' (x+1) z xs
  where sx = sqr x
        l1 = find (\y -> y <= sx && (rem x y == 0)) xs

primes y = primes' 2 y []
primes' :: Integer -> Integer -> [Integer] -> [Integer]
primes' x z xs =
  if x >= z then xs
  else if l1 == Nothing then primes' (x+1) z (xs ++ [x]) else primes' (x+1) z xs
  where sx = sqr x
        l1 = find (\y -> y <= sx && (rem x y == 0)) xs

sqr :: Integer -> Integer
sqr x = toInteger $ ceiling $ sqrt (fromIntegral x)

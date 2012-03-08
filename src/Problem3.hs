main = do
  print $ primes 100
  print $ primes 13195
  print $ primes 600851475143

primes x = filter (\y -> rem x y == 0) xs
	where xs = [2..(x-1)]

--primes y = primes' 2 y []

--primes' :: Int -> Int -> [Int] -> [Int]
--primes' x z xs = 
--	if x == z then [1,2] ++ xs
--  else if isprime then primes' (x+1) z (xs ++ [x]) else primes' (x+1) z xs
--	where l1 = filter (\y -> (rem x y == 0)) xs
--	      isprime = length l1 == 0

import Data.Int
import Data.List

main = do
  a <- primefactors 13195 pl []
  print a
  b <- primefactors 600851475143 pl []
  print b
--  print $ primefactors 13195 pl []
--  print $ primefactors 600851475143 pl []
  where pl = primesList

primefactors :: Integer -> [Integer]-> [Integer] -> IO [Integer]
primefactors x pl xs = do
  print x
  print complete
--  print divided
  print xs
  print (complete * divided)
  print ""
  if (complete * divided) == x then return (xs ++ [x]) else primefactors (quot x divided) pl (xs ++ [divided])
  where div = [2..(quot x 2)]
        divided = head $ filter (\y -> rem x y == 0) div
        complete = foldl (*) 1 xs

primesList :: [Integer]
primesList = primes 500 -- 775146

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
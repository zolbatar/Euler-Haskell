import Data.Int
import Data.List

main = do
  print $ primefactors 100 []
  print $ primefactors 13195 []
  print $ primefactors 600851475143 []

primefactors :: Int64 -> [Int64]-> [Int64]
primefactors x xs = if valid then primefactors (quot x firstdivided) (xs ++ [firstdivided]) else (xs ++ [x])
  where div = [2..(quot x 2)]
        divided = filter (\y -> rem x y == 0) div
        firstdivided = head divided
        valid = length divided > 0

--primes :: Int64 -> [Int64]
--primes x = filter (\y -> rem x y == 0) xs
--  where xs = [2..(quot x 2)]

--primes y = primes' 2 y []

--primes' :: Int -> Int -> [Int] -> [Int]
--primes' x z xs = 
--	if x == z then [1,2] ++ xs
--  else if isprime then primes' (x+1) z (xs ++ [x]) else primes' (x+1) z xs
--	where l1 = filter (\y -> (rem x y == 0)) xs
--	      isprime = length l1 == 0

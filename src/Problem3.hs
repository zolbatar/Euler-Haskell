import Data.Int
import Data.List
import Primes

main = do
  print $ primefactors 13195 13195 pl []
  print $ primefactors 600851475143 600851475143 pl []
  where pl = primesList

primefactors :: Integer -> Integer -> [Integer]-> [Integer] -> [Integer]
primefactors target x pl xs = 
  case divided of Nothing -> (xs ++ [x])
                  Just(d) -> primefactors target (quot x d) pl (xs ++ [d])
  where div = [2..(quot x 2)]
        complete = foldl (*) 1 xs
        divided = find (\y -> rem x y == 0) div

primesList :: [Integer]
primesList = primes 500 -- 775146


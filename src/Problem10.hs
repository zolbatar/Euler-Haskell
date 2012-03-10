import Data.List
import Primes

main = do
  print xss
  where xs = primesFast [2..2000000] 2 2000000
        xss = foldl (+) 0 xs


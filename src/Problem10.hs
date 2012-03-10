import Data.List
import Primes

main = do
  print s
  where xs = primesFast 1999999
        s = foldl (+) 0 xs


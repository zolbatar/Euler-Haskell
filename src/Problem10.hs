import Data.List
import Primes

-- 142913828922
main = do
  print s
  where xs = primesFast 1999999
        s = foldl (+) 0 xs


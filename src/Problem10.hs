import Primes

main = do
  print xs
  print xss
  where xs = primes 200000
        xss = foldl (+) 0 xs
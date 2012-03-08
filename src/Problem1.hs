-- 233168
main = print v2

v1 = do
  let xs3 = [i | i <- [3,6..999], rem i 5 /= 0]
      xs5 = [i | i <- [5,10..999], rem i 3 /= 0]
      xs35 = [i | i <- [1..999], rem i 3 == 0 && rem i 5 == 0]
      xs = xs3 ++ xs5 ++ xs35
  foldl (+) 0 xs

v2 = do
let xs = [i | i <- [1..999], rem i 3 == 0 || rem i 5 == 0]
foldl (+) 0 xs

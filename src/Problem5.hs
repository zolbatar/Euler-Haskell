main = do
  print $ try 20

try :: Integer -> Integer
try r = try' r r

try' :: Integer -> Integer -> Integer
try' x r = if div then x else try' (x + r) r
  where div = evenlyDivisible x r

evenlyDivisible :: Integer -> Integer -> Bool
evenlyDivisible x r = res == 0
  where range = [1..r]
        xs = map (\y -> rem x y) range
        res = foldl (+) 0 xs
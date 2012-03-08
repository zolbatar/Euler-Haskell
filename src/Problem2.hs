-- 4613732
main = do
  print $ foldl (+) 0 xse
  where xs = fib1 [] 4000000
        xse = filter even xs

fib1 :: [Int] -> Int -> [Int]
fib1 [] q = fib1 [2,1] q
fib1 (x:y:xs) q = if (x < q) then fib1 ([x+y,x,y] ++ xs) q else [y] ++ xs

-- No good as we need to do an even check
--fib2 0 = 0
--fib2 1 = 1
--fib2 n = fib2 (n-1) + fib2 (n-2) 
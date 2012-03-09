main = do
  print $ squares 100

squares :: Integer -> Integer
squares r = sso - sq
  where ra = [1..r]
        sq = foldl (+) 0 $ map (\x -> x * x) ra
        ss = (foldl (+) 0 ra)
        sso = ss * ss
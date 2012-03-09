main = do
    print $ map (\(x,y,z) -> (x,y,z,x*y*z)) xs
  where r = 1000
        xs = [(i,j,k) | i <- [1..r], j <- [1..r], k <- [1..r], i < j, j < k, (i*i) + (j*j) == (k*k), i + j + k == 1000]

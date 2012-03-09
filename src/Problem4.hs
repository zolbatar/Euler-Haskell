import Data.List

main = do
  print xs4
  where xs = [(i,j) | i <- [1..999], j <- [1..999]] -- Build all combinations
        xs2 = map (\(x,y) -> (x,y,x * y)) xs-- Now build the multiplied number
        xs3 = filter (\(x,y,z) -> isPalindrome $ show z) xs2
        xs4 = sort $ map (\(x,y,z) -> z) xs3 

isPalindrome :: String -> Bool
isPalindrome x = 
  start == end
  where len = div (length x) 2
        start = take len x
        end = take len $ reverse x
  
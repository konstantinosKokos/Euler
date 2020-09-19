module C20
  (result)
  where

import C3 (primeFactors)
import Data.List (sort)

prime_factors_to_100 = map (primeFactors) [2..100]

remove :: Int -> [Int] -> [Int]
remove _ []   = [1]
remove n (x:xs)
  | x < n     = x:remove n xs
  | x == n    =  xs
  | otherwise = x:xs

remove10 :: [Int] -> [Int]
remove10 xs = if (elem 2 xs) && (elem 5 xs) then
  remove10 (remove 2 (remove 5 xs)) else xs

result =  remove10 $ sort (foldr1 (++) prime_factors_to_100)

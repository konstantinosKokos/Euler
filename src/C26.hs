module C26
  (result, cycleLen)
  where

import C3 (primeFactors)
import Data.Ord (comparing)
import Data.List (maximumBy)

clean :: [Int] -> [Int]
clean xs = filter (\x -> x/=2 && x/=5) xs

cycleLen' :: Int -> Int -> Int
cycleLen' 1 _ = 0
cycleLen' n d
  | new_d == 1  = 1
  | otherwise   = 1 + cycleLen' n new_d
  where new_d = (d * 10) `mod` n

cycleLen :: Int -> Int
cycleLen n = cycleLen' (foldr (*) 1 (clean (primeFactors n))) 1

result = fst $ maximumBy (comparing snd) (map (\x -> (x, cycleLen x)) [1..999])
